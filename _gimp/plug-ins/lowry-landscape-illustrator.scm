;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Landscape Illustrator script  for GIMP 2.4
; Original author: Mark Lowry
;
; Tags: photo, artistic
;
; Author statement:
;
; A GIMP script-fu to generate a painted look to a
; landscape photo.  Sometimes provides an interesting
; effect on portraits and animal shots.
;
; First, if desired it will over-sharpen the image.  This
; helps improve definition in the final product.  A value of 5
; is a good place to start.
;
; Next, it creates a top layer set to Darken Only mode and
; then blurs it.  Varying the blur radius will change
; the effect, as will applying a Levels or Curves adjustment
; to the Darken Only layer.  Just play with it and see
; what you get!
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;
; Created on 5/31/2006 for v.2.2.8
; Revised 10/27/2007 to fix unbound variables (required for v.2.4.0).  Only tested on v.2.4.0
;
; --------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define (script-fu-Land_illust  img drawable blur-rad merge-flag sharpen-flag strength )

   (let* (
         (darken-layer 0)
         (merged-layer 0)
         )

   ; Start an undo group.  Everything between the start and the end
   ; will be carried out if an undo command is issued.

   (gimp-image-undo-group-start img)

   ; SHARPEN FIRST, IF DESIRED

   (if (equal? sharpen-flag TRUE)

       (over_sharpen  img drawable strength)

       ()
   )


   ;; CREATE THE DARKEN ONLY LAYER ;;

   ; Create a new layer

   (set! darken-layer (car (gimp-layer-copy drawable 0)))

   ; Give it a name

   (gimp-drawable-set-name darken-layer "Darken Only layer")

   ; Add the new layer to the image

   (gimp-image-add-layer img darken-layer 0)

   ; Set opacity to 100%

   (gimp-layer-set-opacity darken-layer 100)

   (gimp-layer-set-mode darken-layer DARKEN-ONLY-MODE )

   ;
   ;

   ; Blur the layer

   (if (> blur-rad 0)
       (plug-in-gauss-iir 1 img darken-layer blur-rad 1 1 )
       ()
   )

   ;
   ;

   ; NOW MERGE EVERYTHING DOWN IF DESIRED

   (if (equal? merge-flag TRUE)

       (set! merged-layer (car(gimp-image-merge-down img darken-layer 1 )))

       ()
   )

   (if (equal? merge-flag TRUE)

       (gimp-drawable-set-name merged-layer "Result of Landscape Illustrator")

       ()

   )

   ; Complete the undo group

   (gimp-image-undo-group-end img)

   ; Flush the display

   (gimp-displays-flush)

   )

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (over_sharpen  img drawable strength )

   (let* (
         (matrix 0)
         (divisor 0)
         )

   (set! strength (- 11 (/ strength 5) ) 'double )

     (let* (   (matrix-list '(    0     -.2     -.2     -.2     0
                          -.2   -.5     -1      -.5    -.2
                          -.2   -1     10  -1   -.2
                          -.2   -.5     -1      -.5    -.2
                           0     -.2     -.2     -.2     0  ))
             (channels (make-vector 5 'long ))
           )


     (set! matrix (get-matrix matrix-list))

     (vector-set! channels 0 0 )
     (vector-set! channels 1 1 )
     (vector-set! channels 2 1 )
     (vector-set! channels 3 1 )
     (vector-set! channels 4 0 )

     (vector-set! matrix 12 strength)

     (set! divisor (- strength 8.4))

     (plug-in-convmatrix 1 img drawable 25 matrix 0 1.5999 0 5 channels 0 )


     ; Flush the display

     (gimp-displays-flush)

     )
   )

)

;; Convert maxtrix list (25) into matrix array (5x5)

(define (get-matrix matrix-list)
  (let* (
        (n 0)
        )
  (set! n 25 )
  (define (list-ref l n) (nth n l))
    (let* ((count 0)
           (matrix (cons-array 25 'double)))
      (while (< count 25 )
      (aset matrix count (list-ref matrix-list count))
      (set! count (+ count 1)))
      matrix ; Return the matrix array
    )
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(script-fu-register "script-fu-Land_illust"
      "<Image>/FX-Foundry/Artistic/_Landscape Illustrator"
      "Similar to Landscape Painter, but with a bit of a sketch effect as well.  Over-sharpen, then add Darken Only layer and blur it"
      "Mark Lowry"
      "Technique by Mark Lowry"
      "2006"
      "RGB*, GRAY*"
      SF-IMAGE "Image" 0
      SF-DRAWABLE "Current Layer" 0
      SF-VALUE "Blur radius?"   "15"
      SF-TOGGLE "Merge Layers?"  FALSE
      SF-TOGGLE "Sharpen First?" TRUE
      SF-ADJUSTMENT "Sharpening strength?"  '(5 0 10 1 10 0 0)
 )


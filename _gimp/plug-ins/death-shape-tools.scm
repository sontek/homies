; Draw tools for selections.
;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Selection glow script  for GIMP 2.4
; Copyright (c) 2007 Alexia Death
; Based on Giuseppe Conte Draw shape scripts.
;
; Tags: shapes, selection
;
; Author statement:
;  Draws selected shape using current brush and fg color into current selction.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;
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
; You should have received a of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (script-fu-make-a-polygon image
                    drawable
                    seg
                    rotation
                    randme
                    usebrush
                    on-new-layer)
    (if (= (car (gimp-selection-is-empty image)) TRUE)
    (begin
      (gimp-message "The current image doesn't have a selection.\n\nThis script creates a shape in \nSELECTED AREA of the image."))
    (begin
      (if (= randme TRUE) (set! rotation (rand 360)))
      (let* (
           ;(active-selection (car (gimp-selection-save image)))
                 (selection-bounds (gimp-selection-bounds image))
                 (radius)
                 (select-offset-x (cadr selection-bounds))
                 (select-offset-y (caddr selection-bounds))
                 (select-width (- (cadr (cddr selection-bounds)) select-offset-x))
                 (select-height (- (caddr (cddr selection-bounds)) select-offset-y))
                 (origin-x (round (/ select-width 2)))
                 (origin-y (round (/ select-height 2)))
                 (angstep (* (/ 360 seg) (/ 3.14 180)))
                 (radrot  (* rotation (/ 3.14 180)))
                 (spoints (* 2 (+ seg 1)) ) ;we will have that many drawpoints, x and y to calc.
                 (segment (cons-array spoints 'double))
                 (cnts 0)
                 (drawlayer 0)
             )
            (gimp-selection-none image) ;destroying the selection is better because then there is no risk of cuting on rotate.(make it optional?)
                ;Start an undo group so the process can be undone with one undo
                (gimp-image-undo-group-start image)

                (if (= on-new-layer TRUE)
                    (begin
                        (set! drawlayer (car (gimp-layer-new image
                                                (car (gimp-image-width image))
                                                (car (gimp-image-height image))
                                                RGBA-IMAGE
                                                "polygon layer"
                                                100
                                                NORMAL-MODE)))
                        (gimp-drawable-fill drawlayer TRANSPARENT-FILL)
                        (gimp-image-add-layer image drawlayer -1)

                    )
                    (set! drawlayer drawable)
                )

                ;We need to calculate radius and origin cordinates.
                (if (> select-width select-height) (set! radius origin-y) (set! radius origin-x))
                (set! radius (- radius (round (* 0.15 radius)))) ;saving 15% for feather, brushes etc...
                ;Time to calc.
                (while (< cnts spoints) ;Filling the point array
                    (aset segment cnts       (+ select-offset-x (+ (* radius (cos radrot)) origin-x)))
                    (aset segment (+ cnts 1) (+ select-offset-y (+ (- (* radius (sin radrot))) origin-y)))
                    (set! radrot (+ angstep radrot))
                    (set! cnts (+ cnts +2))
                );end while
                (if (= usebrush TRUE)
                       (gimp-paintbrush drawlayer 0 spoints segment 0 0) (gimp-pencil drawable spoints segment )
                )
                ;Finish the undo group for the process
                (gimp-image-undo-group-end image)

                (gimp-displays-flush)
            )
        )
    )
)

(script-fu-register "script-fu-make-a-polygon"
            "<Image>/FX-Foundry/Shapes/Make a Polygon..."
            "Makes any regular polygon inside the current selection using current fg color and brush. Use Ctrl-F to repeat the script while making new selections. It's FUN :)"
            "Alexia Death"
            "Alexia Death"
            "18.11.2007"
            "RGB RGBA GRAY GRAYA"
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Drawable" 0
            SF-ADJUSTMENT  "Segments" '(3 0 9999 1 10 0 1)
            SF-ADJUSTMENT "Rotation"  '(0 0 360 1 10 0 0)
            SF-TOGGLE "Random rotation" TRUE
            SF-TOGGLE "Use brush instead of pencil" TRUE
            SF-TOGGLE "Draw on new layer" FALSE)


;Make a Star :D
(define (script-fu-make-a-star image
                    drawable
                    seg
                    rotation
                    posrat; secondary point postion between primary points
                    plen ; splike ratio to star
                    randme
                    usebrush
                    on-new-layer)
    (if (= (car (gimp-selection-is-empty image)) TRUE)
    (begin
      (gimp-message "The current image doesn't have a selection.\n\nThis script creates a shape in \nSELECTED AREA of the image."))
    (begin
      (if (= randme TRUE) (set! rotation (rand 360)))
      (let* (
           ;(active-selection (car (gimp-selection-save image)))
                 (selection-bounds (gimp-selection-bounds image))
                 (radius)
                 (select-offset-x (cadr selection-bounds))
                 (select-offset-y (caddr selection-bounds))
                 (select-width (- (cadr (cddr selection-bounds)) select-offset-x))
                 (select-height (- (caddr (cddr selection-bounds)) select-offset-y))
                 (origin-x (round (/ select-width 2)))
                 (origin-y (round (/ select-height 2)))
                 (angstep (* (/ 360 seg) (/ 3.14 180)))
                 (halfstep (/ angstep (/ 100 posrat)))
                 (radrot  (* rotation (/ 3.14 180)))
                 (spoints (* (* 2 (+ seg 1)) 2) ) ;we will have that many drawpoints, x and y to calc.
                 (segment (cons-array spoints 'double))
                 (cnts 0)
                 (drawlayer 0)
             )
            (gimp-selection-none image) ;destroying the selection is better because then there is no risk of cuting on rotate.(make it optional?)
                ;Start an undo group so the process can be undone with one undo
                (gimp-image-undo-group-start image)

                (if (= on-new-layer TRUE)
                    (begin
                        (set! drawlayer (car (gimp-layer-new image
                                                (car (gimp-image-width image))
                                                (car (gimp-image-height image))
                                                RGBA-IMAGE
                                                "star layer"
                                                100
                                                NORMAL-MODE)))
                        (gimp-drawable-fill drawlayer TRANSPARENT-FILL)
                        (gimp-image-add-layer image drawlayer -1)

                    )
                    (set! drawlayer drawable)
                )

                ;We need to calculate radius and origin cordinates.
                (if (> select-width select-height) (set! radius origin-y) (set! radius origin-x))
                (set! radius (- radius (round (* 0.15 radius)))) ;saving 15% for feather, brushes etc...
                ;Time to calc.
                (while (< cnts spoints) ;Filling the point array
                    (aset segment cnts       (+ select-offset-x (+ (* radius (cos radrot)) origin-x)))
                    (aset segment (+ cnts 1) (+ select-offset-y (+ (- (* radius (sin radrot))) origin-y)))
                    (aset segment (+ cnts 2) (+ select-offset-x (+ (* (* radius (/ plen 100)) (cos (+ radrot halfstep))) origin-x)))
                    (aset segment (+ cnts 3) (+ select-offset-y (+ (- (* (* radius (/ plen 100)) (sin (+ radrot halfstep)))) origin-y)))
                    (set! radrot (+ angstep radrot))
                    (set! cnts (+ cnts 4))
                );end while
                (if (= usebrush TRUE)
                       (gimp-paintbrush drawlayer 0 spoints segment 0 0) (gimp-pencil drawable spoints segment )
                )
                ;Finish the undo group for the process
                (gimp-image-undo-group-end image)

                (gimp-displays-flush)
            )
        )
    )
)

(script-fu-register "script-fu-make-a-star"
            "<Image>/FX-Foundry/Shapes/Make a Star..."
            "Makes a star with any number of tips inside the current selection using current fg color and brush. Use Ctrl-F to repeat the script while making new selections. It's FUN :)"
            "Alexia Death"
            "Alexia Death"
            "18.11.2007"
            "RGB RGBA GRAY GRAYA"
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Drawable" 0
            SF-ADJUSTMENT  "Segments" '(5 0 9999 1 10 0 1)
            SF-ADJUSTMENT "Rotation"  '(0 0 360 1 10 0 0)
            SF-ADJUSTMENT "Inner point position"  '(50 1 100 1 10 0 0)
            SF-ADJUSTMENT "Spike length ratio"  '(10 1 100 1 10 0 0)
            SF-TOGGLE "Random rotation" TRUE
            SF-TOGGLE "Use brush instead of pencil" TRUE
            SF-TOGGLE "Draw on new layer" FALSE)

;Some shortcut functions to stars and polygons.
(define (script-fu-make-a-common-star image
                    drawable)
        (script-fu-make-a-star image drawable 5 0 50 20 TRUE TRUE FALSE)
)

(script-fu-register "script-fu-make-a-common-star"
            "<Image>/FX-Foundry/Shapes/Presets/5 Point Star"
            "Makes an ordinary star shape inside the selection on current layer, no questions asked"
            "Alexia Death"
            "Alexia Death"
            "18.11.2007"
            "RGB RGBA GRAY GRAYA"
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Drawable" 0)

(define (script-fu-make-a-triangle image
                    drawable)
  (script-fu-make-a-polygon image drawable 3 0 FALSE TRUE FALSE)
)

(script-fu-register "script-fu-make-a-triangle"
            "<Image>/FX-Foundry/Shapes/Presets/Triangle"
            "Makes an ordinary triangle inside the selection on current layer, no questions asked"
            "Alexia Death"
            "Alexia Death"
            "18.11.2007"
            "RGB RGBA GRAY GRAYA"
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Drawable" 0)

(define (script-fu-make-a-square image
                    drawable)
        (script-fu-make-a-polygon image drawable 4 0 FALSE TRUE FALSE)
)

(script-fu-register "script-fu-make-a-square"
            "<Image>/FX-Foundry/Shapes/Presets/Square"
            "Makes an ordinary square inside the selection on current layer, no questions asked"
            "Alexia Death"
            "Alexia Death"
            "18.11.2007"
            "RGB RGBA GRAY GRAYA"
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Drawable" 0)

(define (script-fu-make-a-pentagon image
                    drawable)
  (script-fu-make-a-polygon image drawable 5 0 FALSE TRUE FALSE)
)

(script-fu-register "script-fu-make-a-pentagon"
            "<Image>/FX-Foundry/Shapes/Presets/Pentagon"
            "Makes an ordinary pentagon inside the selection on current layer, no questions asked"
            "Alexia Death"
            "Alexia Death"
            "18.11.2007"
            "RGB RGBA GRAY GRAYA"
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Drawable" 0)



;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Duotone script  for GIMP 2.4
; Original author: 2004, Alexios Chouchoulas
;
; Tags: photo, color, duotone
;
; Author statement:
;
; A simple script to emulate darkroom sepia toning (and other similar
; toning processes). It is based on how actual sepia toning looks
; like, supplemented by Eric R. Jeschke's GIMP tutorial:
;          http://gimpguru.org/Tutorials/SepiaToning/
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Revision 1.3  2004/02/12 22:12:29  alexios
; Another beta version of GIMP 2.0, another release of Duotone.
;
; Revision 1.2  2004/01/31 17:00:21  alexios
; Added support for Indexed images (they're converted to RGB anyway).
;
; Revision 1.1  2004/01/31 16:52:27  alexios
; Initial revision.
;
; --------------------------------------------------------------------
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define the function:

(define (alexios-set-pt a index x y)
  (prog1
   (aset a (* index 2) x)
   (aset a (+ (* index 2) 1) y)))

(define (alexios-duotone-spline)
  (let* ((a (cons-array 6 'byte)))
    (alexios-set-pt a 0 0 0)
    (alexios-set-pt a 1 128 128)
    (alexios-set-pt a 2 255 0)
    a))

(define (script-fu-duotone    inImage
                inLayer
                inTintColour
                inCopy
        inFlatten
    )
 (let* (
         (theImage 0)
         (theLayer 0)
         (theWidth 0)
         (theHeight 0)
         (mask 0)
        )
          (set! theImage (if (= inCopy TRUE)
               (car (gimp-image-duplicate inImage))
                       inImage)
        )

    (set! theWidth (car (gimp-image-width theImage)))
    (set! theHeight (car (gimp-image-height theImage)))

    (if (< 0 (car (gimp-image-base-type theImage)))
        (gimp-image-convert-rgb theImage))

    ; Do the actual work.

    ; Copy the image.

    (gimp-selection-all theImage)
    (gimp-edit-copy inLayer)

    ; Make the tint layer

    (set! theLayer (car (gimp-layer-new     theImage
                        theWidth
                        theHeight
                        RGBA-IMAGE
                        "Tint"
                        100
                        COLOR-MODE)))

    ; Fill the layer with the tint

    (gimp-context-set-foreground inTintColour)
    (gimp-drawable-fill theLayer FOREGROUND-FILL)

    ; Add the layer to the image

    (gimp-image-add-layer theImage theLayer 0)

    ; Create a mask for the new layer

    (set! mask (car (gimp-layer-create-mask theLayer ADD-WHITE-MASK)))
    (gimp-layer-add-mask theLayer mask)
    (gimp-floating-sel-anchor (car (gimp-edit-paste mask TRUE)))
    (gimp-curves-spline mask HISTOGRAM-VALUE 6 (alexios-duotone-spline))

    ; Flatten the image, if we need to.

    (if (= inFlatten TRUE) (gimp-image-flatten theImage))

    ; Have we been working on a copy? If so display the new image.

    (if (= inCopy TRUE)
        (begin
          (gimp-image-clean-all theImage)
          (gimp-display-new theImage)
          )
        ()
    )

    ; The end.

    (gimp-displays-flush)
    )
)

; Register the function with the GIMP:

(script-fu-register
    "script-fu-duotone"
    _"<Image>/FX-Foundry/Photo/Effects/Simple Duotone..."
    "Produces a duotone photograph.

Some interesting values for the colour are Sepia (162 138 101) and Selenium (229 232 234). Play with the colour saturation for more interesting effects, or uncheck the Flatten box and then modify the new layer's opacity. "
    "Alexios Chouchoulas"
    "2004, Alexios Chouchoulas"
    "31st January 2004"
    "RGB* GRAY* INDEXED*"
    SF-IMAGE       "The Image"      0
    SF-DRAWABLE    "The Layer"      0
    SF-COLOR       _"Tint colour"   '(162 138 101)
    SF-TOGGLE      _"Work on Copy"  TRUE
    SF-TOGGLE      _"Flatten Image" TRUE
)

;;; End Of File.

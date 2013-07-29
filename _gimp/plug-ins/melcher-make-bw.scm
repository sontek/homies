;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; bw-photoscript  for GIMP 2.4
; Original author: Alexander Melcher (a.melchers@planet.nl)
; At xMedia, The Netherlands
;
; Tags: photo, b&w
;
; Author statement:
;
; Creates a black & white photograph style image
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
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


; Define the function

(define (script-fu-xmedia-bw-photo inImage
                        inLayer
                        inDarken
                        inContrast
                        inMottle
                        inDefocus
                        inApplyOn
    )
   (let* (
          (theImage 0)
          (oldSelection 0)
          (theLayer 0)
          (oldRed 0)
          (oldGreen 0)
          (oldBlue 0)
         )
    ; Make a copy if required
    (set! theImage (if (= inApplyOn 0)
                    (car (gimp-image-duplicate inImage))
                    inImage)
    )

    ; Group undo information
    (gimp-image-undo-group-start theImage)

    ; Save the selection
    (set! oldSelection (car (gimp-selection-save theImage)))
    (if (< inApplyOn 2)
        (gimp-selection-none theImage)
        ()
    )

    ; Get the layer
    (set! theLayer (if (= inApplyOn 2)
                    inLayer
                    (car (gimp-image-flatten theImage)))
    )

    ; Save the component state
    (set! oldRed (car (gimp-image-get-component-active theImage 0)))
    (set! oldGreen (car (gimp-image-get-component-active theImage 1)))
    (set! oldBlue (car (gimp-image-get-component-active theImage 2)))

    ; Enable all components
    (gimp-image-set-component-active theImage 0 TRUE)
    (gimp-image-set-component-active theImage 1 TRUE)
    (gimp-image-set-component-active theImage 2 TRUE)

    ; Defocus
    (if (= inDefocus TRUE)
        (plug-in-gauss-rle 1 theImage theLayer 1.5 TRUE TRUE)
        ()
    )

    ; Change the brightness of the red component
    (gimp-image-set-component-active theImage 0 TRUE)
    (gimp-image-set-component-active theImage 1 FALSE)
    (gimp-image-set-component-active theImage 2 FALSE)
    (gimp-brightness-contrast theLayer (- 0 inDarken) 0)

    ; Change the contrast of the green component
    (gimp-image-set-component-active theImage 0 FALSE)
    (gimp-image-set-component-active theImage 1 TRUE)
    (gimp-image-set-component-active theImage 2 FALSE)
    (gimp-brightness-contrast theLayer 0 inContrast)

    ; Mottle
    (if (> inMottle 0)
        (begin
            (gimp-image-set-component-active theImage 0 FALSE)
            (gimp-image-set-component-active theImage 1 FALSE)
            (gimp-image-set-component-active theImage 2 TRUE)
            (plug-in-noisify 1 theImage theLayer TRUE 0 0 0 0.5)
            (plug-in-gauss-rle 1 theImage theLayer 5 TRUE TRUE)
        )
        ()
    )

    ; Enable all components
    (gimp-image-set-component-active theImage 0 TRUE)
    (gimp-image-set-component-active theImage 1 TRUE)
    (gimp-image-set-component-active theImage 2 TRUE)

    ; Desaturate
    (gimp-desaturate theLayer)

    ; Restore the component state
    (gimp-image-set-component-active theImage 0 oldRed)
    (gimp-image-set-component-active theImage 1 oldGreen)
    (gimp-image-set-component-active theImage 2 oldBlue)

    ; Restore the selection
    (gimp-selection-load oldSelection)

    ; Group undo information
    (gimp-image-undo-group-end theImage)

    (if (= inApplyOn 0)
        (begin
            (gimp-image-clean-all theImage)
            (gimp-display-new theImage)
        )
        ()
    )

    ; Force update
    (gimp-displays-flush)

    ; Return
    (list theImage theLayer)
   )
)

; Register script-fu-bw-photo

(script-fu-register
    "script-fu-xmedia-bw-photo"
    "<Image>/FX-Foundry/Photo/Effects/Black & White Photo..."
    "Creates a black & white photograph style image."
    "Alexander Melchers"
    "2002, Alexander Melchers, xMedia"
    "17th November 2002"
    "RGB*"
    SF-IMAGE      "The Image"           0
    SF-DRAWABLE   "The Layer"           0
    SF-ADJUSTMENT "Darken"              '(50 0 127 1 0 0 0)
    SF-ADJUSTMENT "Contrast"            '(50 0 127 1 0 0 0)
    SF-ADJUSTMENT "Mottle"              '(0.5 0 1 0.01 0 1 0)
    SF-TOGGLE     "Defocus"             FALSE
    SF-OPTION     "Apply on"            '("Copy" "Image" "Selection")
)

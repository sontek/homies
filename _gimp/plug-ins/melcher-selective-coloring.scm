;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; selective-colouring script  for GIMP 2.4
; Original author: Alexander Melcher (a.melchers@planet.nl)
; At xMedia, The Netherlands
;
; Tags: photo, selection, color
;
; Author statement:
;
; Allows you to change the overall color or the color balance of a selection
; and all the area around it at the same time, using two different adjustments.
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

(define (script-fu-selective-coloring inImage
                                  inLayer
                                  inIntCR
                                  inIntMG
                                  inIntYB
                                  inIntApply
                                  inIntPreserveLum
                                  inIntColor
                                  inIntColorify
                                  inExtCR
                                  inExtMG
                                  inExtYB
                                  inExtApply
                                  inExtPreserveLum
                                  inExtColor
                                  inExtColorify
                                  inCopy
                                  inFlatten
    )

    (if (= (car (gimp-selection-is-empty inImage)) TRUE)
        (begin
            (gimp-message "This script needs a selection to work on.")
            (list -1 -1)
        )
        (begin
            ; Select the image; make a copy if needed
            (set! theImage (if (= inCopy TRUE)
                           (car (gimp-channel-ops-duplicate inImage))
                           inImage)
            )

            ; Group undo information
            (gimp-undo-push-group-start theImage)

            ; If requested to flatten image, do it now
            (if (= inFlatten TRUE)
                (gimp-image-flatten theImage)
                ()
            )

            ; Select the layer to which to apply the HSV changes
            (set! theLayer (if (= inFlatten TRUE)
                           (aref (cadr (gimp-image-get-layers theImage)) 0)
                           (if (= inCopy TRUE)
                           (car (gimp-image-get-active-layer theImage))
                           inLayer))
            )

            ; Invert the selection
            (gimp-selection-invert theImage)

            ; Colorify
            (if (= inExtColorify TRUE)
                (plug-in-colorify TRUE theImage theLayer inExtColor)
                (gimp-color-balance theLayer inExtApply
                                        inExtPreserveLum inExtCR
                                    inExtMG inExtYB)
            )

            ; Re-invert the selection to regain the original
            (gimp-selection-invert theImage)

            ; Colorify
            (if (= inIntColorify TRUE)
                (plug-in-colorify TRUE theImage theLayer inIntColor)
                (gimp-color-balance theLayer inIntApply
                                        inIntPreserveLum inIntCR
                                    inIntMG inIntYB)
            )

            ; Group undo information
            (gimp-undo-push-group-end theImage)

            ; If a copy was made, show the new image
            (if (= inCopy TRUE)
                (begin
                    (gimp-image-clean-all theImage)
                    (gimp-display-new theImage)
                )
                ()
            )

            ; Force updates
            (gimp-displays-flush)

            ; Return the results
            (list theImage inLayer)
        )
    )
)

; Register script-fu-selective-coloring

(script-fu-register
    "script-fu-selective-coloring"
    "<Image>/FX-Foundry/Selection Effects/xMedia Selective Coloring"
    "Allows you to change the overall color or the color balance of a selection and all the area around it at the same time, using two different adjustments."
    "Alexander Melchers"
    "2002, Alexander Melchers, xMedia"
    "8th November 2002"
    "RGB*"
    SF-IMAGE      "The Image"           0
    SF-DRAWABLE   "The Layer"           0
    SF-ADJUSTMENT "Internal Cyan/Red"   '(0 -100 100 1 1 0 0 0)
    SF-ADJUSTMENT "Magenta/Green"       '(0 -100 100 1 1 0 0 0)
    SF-ADJUSTMENT "Yellow/Blue"         '(0 -100 100 1 1 0 0 0)
    SF-OPTION     "Apply To"            '("Shadows" "Midtones" "Highlights")
    SF-TOGGLE     "Preserve Luminosity" TRUE
    SF-COLOR      "Color"               '(255 255 0)
    SF-TOGGLE     "Colorify"            FALSE
    SF-ADJUSTMENT "External Cyan/Red"   '(0 -100 100 1 1 0 0 0)
    SF-ADJUSTMENT "Magenta/Green"       '(0 -100 100 1 1 0 0 0)
    SF-ADJUSTMENT "Yellow/Blue"         '(0 -100 100 1 1 0 0 0)
    SF-OPTION     "Apply To"            '("Shadows" "Midtones" "Highlights")
    SF-TOGGLE     "Preserve Luminosity" TRUE
    SF-COLOR      "Color"               '(255 255 0)
    SF-TOGGLE     "Colorify"            FALSE
    SF-TOGGLE     "Work on Copy"        FALSE
    SF-TOGGLE     "Flatten Image"       FALSE
)

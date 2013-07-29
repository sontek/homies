;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Duotone/Tritone Simulation, V2.2 for GIMP 2.4
; Original author: Martin Egger (martin.egger@gmx.net)
; (C) 2005, Bern, Switzerland
;
; Tags: photo, duotone
;
; Author statement:
;
; You can find more about toning at
; http://www.gimp.org/tutorials/Sepia_Toning/
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.

;
; Define the function
;
(define (script-fu-Eg-DuotoneSimulation InImage InLayer InType InOpacity InColor InBW InFlatten)
;
; Save history
;
    (gimp-image-undo-group-start InImage)
;
    (let*    (
        (CopyLayer (car (gimp-layer-copy InLayer TRUE)))
        (TintLayer (car (gimp-layer-new InImage (car (gimp-image-width InImage)) (car (gimp-image-height InImage)) RGBA-IMAGE "Tint" InOpacity COLOR-MODE)))
        (Old-FG-Color (car (gimp-palette-get-foreground)))
        )
        (gimp-image-add-layer InImage CopyLayer -1)
        (gimp-drawable-set-name CopyLayer "BW-Copy")
;
; Convert base image to B/W if needed
;
        (if (= InBW TRUE) (gimp-desaturate CopyLayer))
;
; Select the tint color
;
        (cond
;
; Cyano
;
            ((= InType 0) (gimp-palette-set-foreground '(68 174 246)))
;
; Palladium (Yellow)
;
            ((= InType 1) (gimp-palette-set-foreground '(143 153 69)))
;
; Selenium (Magenta)
;
            ((= InType 2) (gimp-palette-set-foreground '(158 79 104)))
;
; Sepia (Brown)
;
            ((= InType 3) (gimp-palette-set-foreground '(167 127 92)))
;
; Sepia (Grey)
;
            ((= InType 4) (gimp-palette-set-foreground '(162 138 101)))
;
; Sepia (Red)
;
            ((= InType 5) (gimp-palette-set-foreground '(184 110 55)))
;
; Sepia (Yellow)
;
            ((= InType 6) (gimp-palette-set-foreground '(181 127 52)))
;
; Silver (Blue)
;
            ((= InType 7) (gimp-palette-set-foreground '(92 153 154)))
;
; Selection
;
            ((= InType 8) (gimp-palette-set-foreground InColor))
        )
;
; Fill the layer with the tint
;
        (gimp-drawable-fill TintLayer FOREGROUND-FILL)
;
; Add the layer to the image
;
        (gimp-image-add-layer InImage TintLayer -1)
;
; Create a mask for the new layer
;
        (let*    (
            (TintMask (car (gimp-layer-create-mask TintLayer ADD-WHITE-MASK)))
            )
            (gimp-layer-add-mask TintLayer TintMask)
            (gimp-selection-all InImage)
            (gimp-edit-copy InLayer)
            (gimp-floating-sel-anchor (car (gimp-edit-paste TintMask TRUE)))
            (gimp-invert TintMask)
        )
;
; Flatten the image, if we need to
;
        (cond
            ((= InFlatten TRUE)
                (begin
                    (gimp-image-merge-down InImage CopyLayer CLIP-TO-IMAGE)
                    (gimp-image-merge-down InImage TintLayer CLIP-TO-IMAGE)
                )
            )
            ((= InFlatten FALSE)
                (begin
                    (if (= InBW FALSE) (gimp-image-remove-layer InImage CopyLayer))
                )
            )
        )
        (gimp-palette-set-foreground Old-FG-Color)
    )
;
; Finish work
;
    (gimp-image-undo-group-end InImage)
    (gimp-displays-flush)
;
)
;
; Register the function with the GIMP
;
(script-fu-register
    "script-fu-Eg-DuotoneSimulation"
    "<Image>/FX-Foundry/Photo/Effects/Eg Duotone Simulation"
    "Simulate Duotones in GIMP"
    "Martin Egger (martin.egger@gmx.net)"
    "2005, Martin Egger, Bern, Switzerland"
    "12.07.2005"
    "RGB*"
    SF-IMAGE    "The Image"    0
    SF-DRAWABLE    "The Layer"    0
    SF-OPTION    "Select Tone"
            '(
                    "Cyano"
                    "Palladium (Yellow)"
                    "Selenium (Magenta)"
                    "Sepia (Brown)"
                    "Sepia (Grey)"
                    "Sepia (Red)"
                    "Sepia (Yellow)"
                    "Silver (Blue)"
                    "Color from selection"
            )
    SF-ADJUSTMENT    "Layer Opacity"    '(65.0 1.0 100.0 1.0 0 2 0)
    SF-COLOR    "Select Color"    '(167 127 92)
    SF-TOGGLE    "Convert to B/W" FALSE
    SF-TOGGLE    "Flatten Image"    FALSE
)
;
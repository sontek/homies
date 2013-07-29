;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Border script for GIMP 2.4
; Original author: Martin Egger (martin.egger@gmx.net)
; (C) 2005, Bern, Switzerland
;
; Tags: border, decor
;
; Author statement:
;
; You can find more about simulating BW at
; http://epaperpress.com/psphoto/
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
(define (script-fu-Eg-Border InImage InLayer InOuterPercentWidth InOuterPercentHeight InInnerPercent InOuterColor InInnerColor InFeather)
;
    (let*    (
        (TheImage (car (gimp-channel-ops-duplicate InImage)))
        (TheLayer (car (gimp-image-flatten TheImage)))
        (TheWidth (car (gimp-image-width TheImage)))
        (TheHeight (car (gimp-image-height TheImage)))
        (outer-border-width (/ (* TheWidth InOuterPercentWidth) 100))
        (outer-border-height (/ (* TheHeight InOuterPercentHeight) 100))
        (inner-border-width (/ (* TheWidth InInnerPercent) 100))
        (inner-border-height inner-border-width)
        (total-border-width (+ inner-border-width outer-border-width))
        (total-border-height (+ inner-border-height outer-border-height))
        (image-width (+ TheWidth  (* 2 total-border-width)))
        (image-height (+ TheHeight (* 2 total-border-height)))
        )
        (gimp-image-undo-disable TheImage)
        (gimp-selection-none TheImage)
        (gimp-drawable-set-name TheLayer "WithBorder")
;
; Generate the border
;
        (gimp-image-resize TheImage image-width image-height total-border-width total-border-height)
;
        (let*    (
            (BorderLayer (car (gimp-layer-new TheImage image-width image-height RGBA-IMAGE "TempLayer" 100 NORMAL-MODE)))
            )
            (gimp-image-add-layer TheImage BorderLayer -1)
            (gimp-edit-clear BorderLayer)
;
            (gimp-rect-select TheImage 0 0 image-width image-height CHANNEL-OP-REPLACE FALSE 0)
            (gimp-rect-select TheImage total-border-width total-border-height TheWidth TheHeight CHANNEL-OP-SUBTRACT FALSE 0)
            (gimp-palette-set-foreground InOuterColor)
            (gimp-edit-fill BorderLayer FOREGROUND-FILL)
;
            (cond
                ((> InInnerPercent 0)
                    (begin
                        (gimp-rect-select TheImage outer-border-width outer-border-height (- image-width  (* outer-border-width 2)) (- image-height (* outer-border-height 2)) CHANNEL-OP-REPLACE InFeather (* 1.2 inner-border-width))
                        (gimp-rect-select TheImage total-border-width total-border-height TheWidth TheHeight CHANNEL-OP-SUBTRACT FALSE 0)
                        (gimp-palette-set-foreground InInnerColor)
                        (gimp-edit-fill BorderLayer FOREGROUND-FILL)
                    )
                )
            )
            (gimp-image-merge-down TheImage BorderLayer CLIP-TO-IMAGE)
        )
;
        (gimp-selection-none TheImage)
        (gimp-display-new TheImage)
        (gimp-image-undo-enable TheImage)
    )
;
; Finish work
;
    (gimp-displays-flush)
;
)
;
; Register the function with the GIMP
;
(script-fu-register
    "script-fu-Eg-Border"
    "<Image>/FX-Foundry/Image Effects/Eg Matt and Border"
    "Generate a border around an image"
    "Martin Egger (martin.egger@gmx.net)"
    "2005, Martin Egger, Bern, Switzerland"
    "26.08.2005"
    "RGB*,GRAY*"
    SF-IMAGE    "The Image"    0
    SF-DRAWABLE    "The Layer"    0
    SF-ADJUSTMENT     "Outer border size (width in percent)" '(15 1.0 100 1.0 0 2 0)
    SF-ADJUSTMENT     "Outer border size (height in percent)" '(15 1.0 100 1.0 0 2 0)
    SF-ADJUSTMENT    "Inner border size (in percent)" '(0.5 0.0 10.0 0.1 0 2 0)
    SF-COLOR "Outer border color" '(255 255 255)
    SF-COLOR "Inner border color" '(0 0 0)
    SF-TOGGLE "Feather inner border" TRUE
)
;
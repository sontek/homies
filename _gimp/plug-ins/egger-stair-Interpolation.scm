;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Stair Interpolation, V2.0 for GIMP 2.4
; Original author: Martin Egger (martin.egger@gmx.net)
; (C) 2005, Bern, Switzerland
;
; Tags: scale
;
;
;
; The image will be enlarged in 10% steps up to the specified factor using
; best available (=cubic) interpolation.
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
(define (script-fu-Eg-StairInterpolation InImage InLayer InFactor)
;
    (let*    (
        (TheImage (car (gimp-channel-ops-duplicate InImage)))
        (TheLayer (car (gimp-image-flatten TheImage)))
        (Step (* 10 (- InFactor 1)))
        (TheWidth (car (gimp-image-width TheImage)))
        (TheHeight (car (gimp-image-height TheImage)))
        (TotWidth (* InFactor TheWidth))
        (TotHeight (* InFactor TheHeight))
        (IncWidth (/ (- TotWidth TheWidth) Step))
        (IncHeight (/ (- TotHeight TheHeight) Step))
        (Counter 1)
        )
        (gimp-image-undo-disable TheImage)
        (gimp-selection-none TheImage)
;
; Resize the image
;
        (while (<= Counter Step)
;
            (let*    (
                (NewWidth (+ TheWidth (* Counter IncWidth)))
                (NewHeight (+ TheHeight (* Counter IncHeight)))
                )
                (gimp-drawable-transform-scale TheLayer 0.0 0.0 NewWidth NewHeight TRANSFORM-FORWARD INTERPOLATION-CUBIC TRUE 3 FALSE)
            )
            (set! Counter (+ Counter 1))
        )
        (gimp-image-resize-to-layers TheImage)
        (gimp-display-new TheImage)
        (gimp-drawable-set-name TheLayer "StairInterpolated")
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
    "script-fu-Eg-StairInterpolation"
    "<Image>/FX-Foundry/Toolbox/Eg Stair Scaleup"
    "Scale up images with minimal cost in quality but larger cost in time. Enlarges the image step by step"
    "Martin Egger (martin.egger@gmx.net)"
    "2005, Martin Egger, Bern, Switzerland"
    "31.05.2005"
    "RGB* GRAY*"
    SF-IMAGE    "The Image"    0
    SF-DRAWABLE    "The Layer"    0
    SF-ADJUSTMENT    "Factor"    '(1.0 1.0 3.0 0.1 0 2 0)
)
;
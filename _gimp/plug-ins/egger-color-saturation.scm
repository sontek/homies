;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Color Saturation script for GIMP 2.4
; Original author: Martin Egger (martin.egger@gmx.net)
; (C) 2005, Bern, Switzerland
;
; Tags: saturation, color
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;  august 2007 - fixed for gimp 2.4
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Define the function
;
(define (script-fu-Eg-ColorSaturation InImage InLayer InIntensity InFlatten)
;
; Save history
;
    (gimp-image-undo-group-start InImage)
;
    (let*    (
        (factor (* InIntensity .025))
        (plus (+ 1 (* 2 factor)))
        (minus (* -1 factor))
        (ColorLayer (car (gimp-layer-copy InLayer TRUE)))
        )
        (gimp-image-add-layer InImage ColorLayer -1)
;
; Apply new color mappings to image
;
        (plug-in-colors-channel-mixer TRUE InImage ColorLayer FALSE plus minus minus minus plus minus minus minus plus)
;
; Flatten the image, if we need to
;
        (cond
            ((= InFlatten TRUE) (gimp-image-merge-down InImage ColorLayer CLIP-TO-IMAGE))
            ((= InFlatten FALSE) (gimp-drawable-set-name ColorLayer "Saturated"))
        )
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
    "script-fu-Eg-ColorSaturation"
    "<Image>/FX-Foundry/Color/Eg Color Saturation"
    "Saturate or desaturate color images"
    "Martin Egger (martin.egger@gmx.net)"
    "2005, Martin Egger, Bern, Switzerland"
    "15.05.2005"
    "RGB*"
    SF-IMAGE    "The Image"    0
    SF-DRAWABLE    "The Layer"    0
    SF-ADJUSTMENT    "Intensity"    '(1 -7 7 0.5 0 2 0)
    SF-TOGGLE    "Flatten Image"    FALSE
)
;

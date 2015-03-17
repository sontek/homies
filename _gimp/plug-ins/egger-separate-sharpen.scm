;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Separate Sharpening, V1.1 for GIMP 2.4
; Original author: Martin Egger (martin.egger@gmx.net)
; (C) 2005, Bern, Switzerland
;
; Tags: photo, sharpen
;
;
; You can find more about Separate Sharpening at
; http://thomas-stoelting.de/photoshop.html
;
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
(define (script-fu-Eg-SeparateSharpen InImage InLayer InRadius InAmount InThreshold InFlatten)
;
; Save history
;
    (gimp-image-undo-group-start InImage)
;
    (let*    (
        (SharpenLightsLayer (car (gimp-layer-copy InLayer TRUE)))
        )
;
        (gimp-image-add-layer InImage SharpenLightsLayer -1)
           (gimp-layer-set-mode SharpenLightsLayer LIGHTEN-ONLY-MODE)
           (gimp-layer-set-opacity SharpenLightsLayer 40)
        (plug-in-unsharp-mask TRUE InImage SharpenLightsLayer InRadius InAmount InThreshold)
;
        (let*    (
            (SharpenShadowsLayer (car (gimp-layer-copy SharpenLightsLayer TRUE)))
            )
            (gimp-image-add-layer InImage SharpenShadowsLayer -1)
            (gimp-layer-set-mode SharpenShadowsLayer DARKEN-ONLY-MODE)
            (gimp-layer-set-opacity SharpenShadowsLayer 100)

;
; Flatten the image, if we need to
;
            (cond
                ((= InFlatten TRUE)
                    (begin
                        (let*    (
                            (SharpenLightsLayer (car (gimp-image-merge-down InImage SharpenShadowsLayer CLIP-TO-IMAGE)))
                            )
                            (gimp-image-merge-down InImage SharpenLightsLayer CLIP-TO-IMAGE)
                        )
                    )
                )
                ((= InFlatten FALSE)
                    (begin
                        (gimp-drawable-set-name SharpenLightsLayer "Sharpened Lights")
                        (gimp-drawable-set-name SharpenShadowsLayer "Sharpened Shadows")
                    )
                )
            )
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
(script-fu-register
    "script-fu-Eg-SeparateSharpen"
    "<Image>/FX-Foundry/Photo/Sharpen/Eg Separate Lights&Shadows Sharpen"
    "Separate Sharpening (Lights&Shadows)"
    "Martin Egger (martin.egger@gmx.net)"
    "2005, Martin Egger, Bern, Switzerland"
    "14.06.2005"
    "RGB* GRAY*"
    SF-IMAGE    "The Image"        0
    SF-DRAWABLE    "The Layer"        0
    SF-ADJUSTMENT    "Radius of USM"        '(3.0 0.0 50.0 1 0 2 0)
    SF-ADJUSTMENT    "Amount of USM"        '(1.0 0.0 5.0 0.5 0 2 0)
    SF-ADJUSTMENT    "Threshold"        '(0.0 0.0 50.0 1.0 0 2 0)
    SF-TOGGLE    "Flatten Image"        FALSE
)
;
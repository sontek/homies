;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Sensor Noise Reduction script  for GIMP 2.4
; Original author: Martin Egger (martin.egger@gmx.net)
; (C) 2005, Bern, Switzerland
;
; Tags: photo, noise
;
; Author statement:
;
; You can find more about ISO Noise Reduction at
; http://www.gimpguru.org/Tutorials/ReducingCCDNoise/
; but this script uses a different method (masking the edges and then
; blurring the individual color channels or the lumimance channel only).
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;  02.11.2007 - fixed for gimp 2.4 by Alexia Death. Lab
;               layer order for recompose has inversed between versions
;               This script WILL NOT work right for 2.2
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

;
; Define the function
;
(define (script-fu-Eg-ISONoiseReduction InImage InLayer InType InOpacity InRadius InFlatten)
;
; Save history
;
    (gimp-image-undo-group-start InImage)
;
    (let*    (
        (NoiseLayer (car (gimp-layer-copy InLayer TRUE)))
        (TempLayer (car (gimp-layer-copy InLayer TRUE)))
        (InDelta (* InRadius 7 ))
        (RadiusRB (* InRadius 1.5))
        (DeltaRB (* InDelta 2))
        )
;
        (gimp-image-add-layer InImage NoiseLayer -1)
        (gimp-image-add-layer InImage TempLayer -1)
;
; Find edges, Radius = 10, Warpmode = Smear (1), Edgemode = Laplace (5)
;
        (plug-in-edge TRUE InImage TempLayer 10 1 5)
        (gimp-desaturate TempLayer)
        (gimp-invert TempLayer)
        (plug-in-gauss TRUE InImage TempLayer 10.0 10.0 0)
        (gimp-selection-all InImage)
        (gimp-edit-copy TempLayer)
        (gimp-image-remove-layer InImage TempLayer)
;
        (let*    (
            (NoiseLayerMask (car (gimp-layer-create-mask NoiseLayer ADD-SELECTION-MASK)))
            )
            (gimp-image-add-layer-mask InImage NoiseLayer NoiseLayerMask)
            (gimp-floating-sel-anchor (car (gimp-edit-paste NoiseLayerMask FALSE)))
            (gimp-selection-none InImage)
;
; Select method
;
            (cond
;
; Blur seperate RGB channels, use different radius/delta for Red/Blue and Green
;
                ((= InType 0)
                    (begin
                        (gimp-image-set-component-active InImage RED-CHANNEL TRUE)
                        (gimp-image-set-component-active InImage GREEN-CHANNEL FALSE)
                        (gimp-image-set-component-active InImage BLUE-CHANNEL FALSE)
                        (plug-in-sel-gauss TRUE InImage NoiseLayer RadiusRB DeltaRB)
;
                        (gimp-image-set-component-active InImage RED-CHANNEL FALSE)
                        (gimp-image-set-component-active InImage GREEN-CHANNEL TRUE)
                        (plug-in-sel-gauss TRUE InImage NoiseLayer InRadius InDelta)
;
                        (gimp-image-set-component-active InImage GREEN-CHANNEL FALSE)
                        (gimp-image-set-component-active InImage BLUE-CHANNEL TRUE)
                        (plug-in-sel-gauss TRUE InImage NoiseLayer RadiusRB DeltaRB)
;
                        (gimp-image-set-component-active InImage RED-CHANNEL TRUE)
                        (gimp-image-set-component-active InImage GREEN-CHANNEL TRUE)
                    )
                )
;
; Blur luminance channel
;
                ((= InType 1)
                    (begin
                        (let*    (
                            (OrigLayer (cadr (gimp-image-get-layers InImage)))
                            (LABImage (car (plug-in-decompose TRUE InImage InLayer "LAB" TRUE)))
                               (LABLayer (cadr (gimp-image-get-layers LABImage)))
                               (LLayer (car (gimp-layer-copy InLayer TRUE)))
                               )
;
                               (gimp-image-add-layer InImage LLayer -1)
                               (gimp-selection-all LABImage)
                               (gimp-edit-copy (aref LABLayer 0))
                               (gimp-floating-sel-anchor (car (gimp-edit-paste LLayer FALSE)))
                               (plug-in-sel-gauss TRUE InImage LLayer RadiusRB DeltaRB)
                               (gimp-selection-all InImage)
                               (gimp-edit-copy LLayer)
                               (gimp-image-remove-layer InImage LLayer)
                               (gimp-floating-sel-anchor (car (gimp-edit-paste (aref LABLayer 0) FALSE)))
                               (let*    (
                                   (CompImage (car (plug-in-drawable-compose TRUE LABImage (aref LABLayer 0) (aref LABLayer 1) (aref LABLayer 2) 0 "LAB")))
                                   (CompLayer (cadr (gimp-image-get-layers CompImage)))
                                   )
                                   (gimp-selection-all CompImage)
                                   (gimp-edit-copy (aref CompLayer 0))
                                   (gimp-floating-sel-anchor (car (gimp-edit-paste NoiseLayer FALSE)))
                                   (gimp-image-delete CompImage)
                               )
                               (gimp-image-delete LABImage)
                        )
                       )
                )
;
; GMIP despeckle plugin
;
                ((= InType 2) (plug-in-despeckle TRUE InImage NoiseLayer InRadius 1 7 248))
            )
            (gimp-layer-set-opacity NoiseLayer InOpacity)
        )
;
; Flatten the image, if we need to
;
        (cond
            ((= InFlatten TRUE) (gimp-image-merge-down InImage NoiseLayer CLIP-TO-IMAGE))
            ((= InFlatten FALSE) (gimp-drawable-set-name NoiseLayer "Noisefree"))
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
    "script-fu-Eg-ISONoiseReduction"
    "<Image>/FX-Foundry/Photo/Enhancement/Eg ISO Noise Reduction"
    "Reduce sensor noise at high ISO values"
    "Martin Egger (martin.egger@gmx.net)"
    "2005, Martin Egger, Bern, Switzerland"
    "1.06.2005"
    "RGB* GRAY*"
    SF-IMAGE    "The Image"        0
    SF-DRAWABLE    "The Layer"        0
    SF-OPTION     "Noise Reduction Method"
            '(
                        "RGB channel blurring (faster)"
                        "Luminance channel blurring (slower)"
                        "GMIP Despeckle plugin"
            )
    SF-ADJUSTMENT    "Layer Opacity"    '(70.0 1.0 100.0 1.0 0 2 0)
    SF-ADJUSTMENT    "Strength of Blurring"    '(5 1.0 10.0 0.5 0 2 0)
    SF-TOGGLE    "Flatten Image"    FALSE
)
;
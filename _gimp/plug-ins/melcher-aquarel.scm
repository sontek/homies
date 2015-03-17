;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; xMedia quarel script  for GIMP 2.4
; Original author: Alexander Melcher (a.melchers@planet.nl)
; At xMedia, The Netherlands
;
; Tags: photo, artistic
;
; Author statement:
;
; Gives the image an aquareled look
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

(define (script-fu-aquarel inImage
                       inLayer
                       inWaterEdges
                       inBrushSize
                       inAccuracy
    )
    (let* (
           (textureLayer 0)
           (oldSelection 0)
           (oldPattern 0)
          )
    ; Group undo information
    (gimp-image-undo-group-start inImage)

        ; Water edge effect
        (plug-in-gauss-rle 1 inImage inLayer inWaterEdges 1 1)
        (plug-in-gauss-rle 1 inImage inLayer inWaterEdges 1 1)

        ; Add a brushed effect
        (plug-in-oilify 1 inImage inLayer inBrushSize 1)
        (if (= inAccuracy 0)
                (plug-in-oilify 1 inImage inLayer inBrushSize 1)
                ()
        )

        ; Enhance edges
        (plug-in-sharpen 1 inImage inLayer inWaterEdges)

        ; Enhance contrast
        (gimp-brightness-contrast inLayer 0 50)

    ; Add a paper texture
    (set! textureLayer (car (gimp-layer-new inImage
                              (car (gimp-image-width inImage))
                              (car (gimp-image-height inImage))
                              (car (gimp-drawable-type-with-alpha inLayer))
                              "Paper Texture" 100 OVERLAY-MODE)))

    (gimp-image-add-layer inImage textureLayer -1)
        (set! oldSelection (car (gimp-selection-save inImage)))
    (gimp-selection-all inImage)
    (gimp-edit-clear textureLayer)
        (set! oldPattern (car (gimp-context-get-pattern)))
        (gimp-context-set-pattern "Paper")
        (gimp-edit-bucket-fill textureLayer PATTERN-BUCKET-FILL NORMAL-MODE 100 0 0 0 0)
        (gimp-context-set-pattern oldPattern)
        (gimp-selection-load oldSelection)
        (if (= (car (gimp-selection-is-empty inImage)) TRUE)
                ()
                (begin
                        (gimp-selection-invert inImage)
                        (gimp-edit-clear textureLayer)
                        (gimp-selection-load oldSelection)
                )
        )

    ; Group undo information
    (gimp-image-undo-group-end inImage)

    ; Force update
    (gimp-displays-flush)

    ; Return
    (list inLayer)
    )
)

; Register script-fu-aquarel

(script-fu-register
    "script-fu-aquarel"
    "<Image>/FX-Foundry/Artistic/Aquarel..."
    "Gives the image an aquareled look."
    "Alexander Melchers"
    "2002, Alexander Melchers, xMedia"
    "29th March 2003"
    "RGB* GRAY*"
    SF-IMAGE      "The Image"         0
    SF-DRAWABLE   "The Layer"         0
    SF-ADJUSTMENT "Water Edges"       '(7 3 50 1 1 0 0)
    SF-ADJUSTMENT "Brush Size"        '(7 3 50 1 1 0 0)
    SF-ADJUSTMENT "Accuracy"          '(0 0 1 1 1 0 0)
)

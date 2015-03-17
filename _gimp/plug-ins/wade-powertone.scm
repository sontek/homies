;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Powertone script  for GIMP 2.4
; Created by Art Wade
;
; Tags: color, photo, artistic, tone
;
; Author statement:
;
; This script is based on a tutorial presented by gimptalk.com member,
; lylejk (http://www.gimptalk.com/forum/topic/Powertoning-Very-Simple-Technique-26500-1.html).

; It takes an existing image's active layer and makes two copies: setting the Middle copy
; to Saturation and the Top copy to Value.  The Original Layer is then filled with your
; choice of color.  Gives a very dramatic effect and, depending on the image, somewhat
; resembles an Infrared Photograph.

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



; Define the Function

(define (power-toning inImage inLayer inColor inMerge)


; Declare the Variables

    (let*
        (
            (width (car (gimp-image-width inImage)))
            (height (car (gimp-image-height inImage)))
            (saturation-layer (car (gimp-layer-copy inLayer TRUE)))
            (value-layer (car (gimp-layer-copy inLayer TRUE)))
        )


; Allow for the foreground color to be reset to the value used prior to running the script

(gimp-context-push)

; Begin Undo Group

(gimp-undo-push-group-start inImage)

; Add Saturation and Value Layers to the Image and change names & blend modes accordingly

(gimp-image-add-layer inImage saturation-layer -1)
(gimp-layer-set-mode saturation-layer SATURATION-MODE)
(gimp-drawable-set-name saturation-layer "Saturation Layer")

(gimp-image-add-layer inImage value-layer -1)
(gimp-layer-set-mode value-layer VALUE-MODE)
(gimp-drawable-set-name value-layer "Value Layer")

; Change the active layer to the background layer

(gimp-image-set-active-layer inImage inLayer)

; Set the background color to that chosen by the user and fill the background layer

(gimp-context-set-foreground inColor)
(gimp-edit-fill inLayer FOREGROUND-FILL)

; Merge the layers if requested by the user

(if (= inMerge TRUE)
(gimp-image-merge-visible-layers inImage CLIP-TO-IMAGE))
(set! inLayer (car (gimp-image-get-active-layer inImage)))

; Update display

(gimp-displays-flush)

; End Undo Group

(gimp-undo-push-group-end inImage)

; Return GIMP defaults

(gimp-context-pop)

)
)



(script-fu-register "power-toning"
            "<Image>/FX-Foundry/Artistic/Power Toning..."
            "Adds a neutral gray and color of choice to an existing image. Almost an IR effect."
            "Art Wade"
            "Art Wade"
            "2007"
            "RGB*"
            SF-IMAGE         "Image"                 0
            SF-DRAWABLE     "Drawable"                 0
            SF-COLOR         "Background Color"         '(0 0 0)
        SF-TOGGLE         "Merge Layers?"             FALSE

)



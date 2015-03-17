;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; diffusion filter script  for GIMP 2.4
; Original author: Antoine Tissier
; http://antoine.tissier.lost-oasis.net/gimp/
;
; Tags: effect, filter
;
; Author statement:
;
; This 'script-fu' fakes a diffusion filter.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Changelog
; 0.2->0.3 :
;    the intensity parameter has been added
;    the layers are now renamed
;    an intensity mask has been added
;    comments added
; 0.1->0.2 :
;     english translation
;     the level parametre has been added
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


(define (photo-diffusion_filter image drawable levels radius intensity negative)
    (let* (
        (width (car (gimp-image-width image)))
        (height (car (gimp-image-height image)))
        (blur_layer  (car (gimp-image-get-active-layer image)))
        (old_fg_color (car (gimp-context-get-foreground)))
        (old_bg_color (car (gimp-context-get-background)))
        (grey_color (/ (* (abs (- 100 intensity)) 255) 100)))

    (gimp-image-undo-group-start image)

    ;duplication of the background layer
    (gimp-selection-all image)
    (gimp-edit-copy drawable)
    (let ((floating-sel (car (gimp-edit-paste drawable FALSE))))
    (gimp-floating-sel-to-layer floating-sel))

    ;the foreground layer mode is changed
    (let ((orig_layer (car (gimp-image-get-active-layer image))))
    (gimp-drawable-set-name orig_layer "original")

    (if (= negative FALSE)
        (gimp-layer-set-mode orig_layer OVERLAY-MODE))

    (if (= negative TRUE)
        (gimp-layer-set-mode orig_layer MULTIPLY-MODE))

    ;duplication of the foreground layer
    (let ((floating-sel (car (gimp-edit-paste drawable FALSE))))
    (gimp-floating-sel-to-layer floating-sel))
    (let ((inte_layer (car (gimp-image-get-active-layer image))))
    (gimp-drawable-set-name inte_layer "intensity")
    (gimp-layer-set-mode inte_layer NORMAL-MODE)

    ;creation of the intensity mask
    ;(it is better to create an editable mask than just using opacity )
    (gimp-context-set-background (list grey_color grey_color grey_color))
    (let ((inte_mask (car (gimp-layer-create-mask inte_layer 0))))
    (gimp-layer-add-mask inte_layer inte_mask)
    (gimp-edit-fill inte_mask BACKGROUND-FILL)

    ;the background layer is blurred and brigthened
    (gimp-drawable-set-visible orig_layer FALSE)
    (gimp-drawable-set-visible inte_layer FALSE)
    (gimp-image-set-active-layer image blur_layer)
    (plug-in-gauss-iir2 TRUE image drawable radius radius)
    (gimp-levels drawable 0 0 255 levels 0 255)
    (gimp-drawable-set-name blur_layer "blur")

    (gimp-drawable-set-visible orig_layer TRUE)
    (gimp-drawable-set-visible inte_layer TRUE)
    (gimp-context-set-background old_bg_color)
    (gimp-context-set-foreground old_fg_color)
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)))))
)

(script-fu-register "photo-diffusion_filter"
                                "<Image>/FX-Foundry/Photo/Effects/Diffusion filter"
                                "Fakes a diffusion filter"
                                "Antoine Tissier"
                                "Antoine Tissier"
                                "September 2003"
                                "RGB RGBA GRAY GRAYA"
                                SF-IMAGE "Image" 0
                                SF-DRAWABLE "Drawable" 0
                                SF-ADJUSTMENT "Levels" '(1.5  0.1  10   1  10   1   0)
                                SF-ADJUSTMENT "Radius" '( 15    1 100   1   1   0   0)
                                SF-ADJUSTMENT "Intensity" '(75  1 100   1   1   0   0)
                                SF-TOGGLE "Negative" FALSE )

;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Create Contrast Mask script  for GIMP 2.4
; Created by www.tankedup-imaging.com
;
; Tags: contrast, mask, exposure, photo
;
; Author statement:
;
; A Script-Fu script to correct the exposure of dark areas
; without overexposing bright areas

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



(define (script-fu-contrast img drawable gblur opacity-mask)
  (let* (
        (hue 0)
        (saturation 0)
        (contrast 0)
        )
    (gimp-undo-push-group-start img)

    ; Copy image, add as new layer, set mode to hue
    (set! hue (car (gimp-layer-copy drawable 1)))
    (gimp-image-add-layer img hue -1)
    (gimp-layer-set-name hue "hue")
    (gimp-layer-set-mode hue HUE)

    ; Copy image, add as new layer, set mode to saturation
    (set! saturation (car (gimp-layer-copy drawable 1)))
    (gimp-image-add-layer img saturation -1)
    (gimp-layer-set-name saturation "saturation")
    (gimp-layer-set-mode saturation SATURATION)

    ; Copy image, add as new layer
    (set! contrast (car (gimp-layer-copy drawable 1)))
    (gimp-image-add-layer img contrast -1)
    (gimp-layer-set-name contrast "contrast")

    ; Desaturate, invert, gaussian-blur, set opacity, set mode to overlay
    (gimp-desaturate contrast)
    (gimp-invert contrast)
    (plug-in-gauss-iir 1 img contrast gblur 1 1)
    (gimp-layer-set-opacity contrast opacity-mask)
    (gimp-layer-set-mode contrast OVERLAY)

    ; Cleanup
    (gimp-undo-push-group-end img)
    (gimp-displays-flush)
   )
)

(script-fu-register "script-fu-contrast"
        _"<Image>/FX-Foundry/Photo/Enhancement/_Contrast Overlay"
        "Create a contrast layer with hue and saturation copies of original image"
        "www.tankedup-imaging.com"
        "Tanked Up Underwater Imaging"
        "06 July 2006"
        ""
        SF-IMAGE "Image" 0
        SF-DRAWABLE "Drawable" 0
        SF-ADJUSTMENT   _"Blur Radius" '(20 1 200 1 1 1 0 1)
        SF-ADJUSTMENT "Opacity" '(80 0 100 1 20 0 0)
)

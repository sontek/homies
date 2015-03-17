;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Soft focus script script  for GIMP 2.4
; Copyright (C) 2001 Iccii <iccii@hotmail.com>
;
; Tags: photo, artistic
;
; Author statement:
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; version 0.1  by Iccii 2001/07/22
;     - Initial relase
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

    ;;
(define (script-fu-soft-focus
            img
            drawable
            blur
    )
  (let* (
     (layer-copy (car (gimp-layer-copy drawable TRUE)))
     (layer-mask (car (gimp-layer-create-mask layer-copy ADD-WHITE-MASK)))
        )

    ;;
    (gimp-image-undo-group-start img)
    (gimp-image-add-layer img layer-copy -1)
    (gimp-layer-add-mask layer-copy layer-mask)
    (gimp-edit-copy layer-copy)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-mask 0)))
    (gimp-layer-remove-mask layer-copy MASK-APPLY)
    (plug-in-gauss-iir2 1 img layer-copy blur blur)
    (gimp-layer-set-opacity layer-copy 80)
    (gimp-layer-set-mode layer-copy SCREEN-MODE)

    ;; Clean up
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
  "script-fu-soft-focus"
  "<Image>/FX-Foundry/Photo/Effects/Soft Focus..."
  "Soft focus effect"
  "Iccii <iccii@hotmail.com>"
  "Iccii"
  "2001, Jul"
  "RGB* GRAYA"
  SF-IMAGE      "Image"        0
  SF-DRAWABLE   "Drawable"    0
  SF-ADJUSTMENT _"Blur Amount"  '(10 1 100 1 10 0 0)
)

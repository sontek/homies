
;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Water paint effect script  for GIMP 2.4
; Original author: Iccii <iccii@hotmail.com>
;
; Tags: artistic
;
; Author statement:
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; version 0.1  2001/04/15 iccii <iccii@hotmail.com>
;     - Initial relased
; version 0.1a 2001/07/20 iccii <iccii@hotmail.com>
;     - more simple
; Receved as completely broken, doing just gausian blur. Fixed to do something
; that may have been the authors intent.
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

(define (script-fu-water-paint-effect
            inImage
            inDrawable
            inEffect
    )
  (let* (
        (theNewlayer)
        )
      (gimp-image-undo-group-start inImage)
    (set! theNewlayer (car (gimp-layer-copy inDrawable 1)))
      (plug-in-gauss-iir2 1 inImage inDrawable inEffect inEffect)

      (gimp-image-add-layer inImage theNewlayer -1)
      (plug-in-laplace 1 inImage theNewlayer)
      (gimp-layer-set-mode theNewlayer SUBTRACT-MODE)
      (gimp-image-merge-down inImage theNewlayer EXPAND-AS-NECESSARY)

      (gimp-image-undo-group-end inImage)
      (gimp-displays-flush)
    )
)

(script-fu-register
    "script-fu-water-paint-effect"
    "<Image>/FX-Foundry/Artistic/Water Paint Effect..."
    "draw with water paint effect"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "Jul, 2001"
    "RGB*, GRAY*"
    SF-IMAGE    "Image"        0
    SF-DRAWABLE    "Drawable"    0
    SF-ADJUSTMENT    "Effect Size (pixels)"    '(5 0 32 1 10 0 0)
)

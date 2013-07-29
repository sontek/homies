;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Copy mask script  for GIMP 2.4
; Created by Saul Goode
;
; Tags: multilayer, mask, tool
;
; Author statement:
;
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


(define (script-fu-copy-mask-to-layers image drawable)
  (define (visible-layers img)
    (let* (
        (all-layers (gimp-image-get-layers img))
        (i (car all-layers))
        (viewable ())
        (tmp FALSE))
      (set! all-layers (cadr all-layers))
      (while (> i 0)
        (set! tmp (car (gimp-drawable-get-visible (aref all-layers (- i 1)))))
        (if (= tmp TRUE)
          (set! viewable (append viewable (cons (aref all-layers (- i 1)) 'byte))))
        (set! i (- i 1)))
      viewable))
  (let* (
      (active-layer (car (gimp-image-get-active-layer image)))
      (source-layer 0)
      (source-mask 0)
      (layers 0)
      (mask 0))
   (set! layers (visible-layers image))
   (gimp-image-undo-group-start image)
   (set! source-layer (car (gimp-layer-new-from-drawable active-layer image)))
    (gimp-image-add-layer image source-layer -1)
    (set! source-mask (car (gimp-layer-get-mask source-layer)))
    (if (= source-mask -1)
      (begin
        (set! source-mask (car (gimp-layer-create-mask source-layer ADD-COPY-MASK)))
        (gimp-layer-add-mask source-layer source-mask)))
    (while (car layers)
      (if (= (car (gimp-layer-get-mask (car layers))) -1)
        (if (= (car (gimp-drawable-has-alpha (car layers))) 1)
          (set! mask (car (gimp-layer-add-mask (car layers) source-mask)))))
      (set! layers (cdr layers)))
  (gimp-image-remove-layer image source-layer)
  (gimp-image-undo-group-end image)
  (gimp-displays-flush)))

(script-fu-register "script-fu-copy-mask-to-layers"
 "<Image>/FX-Foundry/Multi-Layer Tools/_Copy mask to all layers"
 "Copy the mask from the current layer to all visible layers"
 "Saul Goode"
 "Saul Goode"
 "6/14/2006"
 ""
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 )

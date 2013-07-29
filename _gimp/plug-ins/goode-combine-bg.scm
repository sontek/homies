;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Combine with background script  for GIMP 2.4
; Created by Saul Goode
;
; Tags: animation, tool
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


(define (script-fu-anim-combine-background image drawable)
  (define (get-all-layers img)
    (let* (
        (all-layers (gimp-image-get-layers img))
        (i (car all-layers))
        (bottom-to-top ())
        )
      (set! all-layers (cadr all-layers))
      (while (> i 0)
        (set! bottom-to-top (append bottom-to-top (cons (aref all-layers (- i 1)) '())))
        (set! i (- i 1))
        )
      bottom-to-top
      )
    )
  (gimp-image-undo-group-start image)
  (let* (
      (layers (get-all-layers image))
      (bg-layer (car layers))
      (base-layer 0)
      (layer-name 0)
      (pos)
      )
    (set! layers (cdr layers))
    (set! pos  (length layers))
    (while (pair? layers)
      (gimp-drawable-set-visible (car layers) TRUE)
      (set! layer-name (car (gimp-drawable-get-name (car layers))))
      (set! base-layer (car (gimp-layer-new-from-drawable bg-layer image)))
      (gimp-image-add-layer image base-layer pos)
      (gimp-drawable-set-visible base-layer TRUE)
      (set! base-layer (car (gimp-image-merge-down image (car layers) EXPAND-AS-NECESSARY)))
      (gimp-drawable-set-name base-layer layer-name)
      (set! pos (- pos 1))
      (set! layers (cdr layers))
      )
    )
  (gimp-image-undo-group-end image)
  (gimp-displays-flush)
  )

(script-fu-register "script-fu-anim-combine-background"
 "<Image>/FX-Foundry/Animation/_Combine with background"
 "Combine each layer of the image with a copy of the background layer"
 "Saul Goode"
 "Saul Goode"
 "4/22/2007"
 ""
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 )

;; Overlay each layer of the image with a copy of the background layer

(define (script-fu-anim-overlay-background image drawable)
  (define (get-all-layers img)
    (let* (
        (all-layers (gimp-image-get-layers img))
        (i (car all-layers))
        (bottom-to-top ())
        )
      (set! all-layers (cadr all-layers))
      (while (> i 0)
        (set! bottom-to-top (append bottom-to-top (cons (aref all-layers (- i 1)) '())))
        (set! i (- i 1))
        )
      bottom-to-top
      )
    )
  (gimp-image-undo-group-start image)
  (let* (
      (layers (get-all-layers image))
      (bg-layer (car layers))
      (over-layer 0)
      (layer-name 0)
      )
    (set! layers (cdr layers))
    (while (pair? layers)
      (gimp-drawable-set-visible (car layers) TRUE)
      (gimp-image-set-active-layer image (car layers))
      (set! over-layer (car (gimp-layer-new-from-drawable bg-layer image)))
      (gimp-image-add-layer image over-layer -1)
      (gimp-drawable-set-visible over-layer TRUE)
      (gimp-image-merge-down image over-layer EXPAND-AS-NECESSARY)
      (set! layers (cdr layers))
      )
    )
  (gimp-image-undo-group-end image)
  (gimp-displays-flush)
  )

(script-fu-register "script-fu-anim-overlay-background"
 "<Image>/FX-Foundry/Animation/_Overlay background"
 "Overlay each layer of the image with a copy of the background layer"
 "Saul Goode"
 "Saul Goode"
 "4/22/2007"
 ""
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 )

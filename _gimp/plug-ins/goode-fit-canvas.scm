;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Fit Canvas To Layer script  for GIMP 2.4
; Created by Saul Goode
;
; Tags: tool, canvas, size
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

(define (script-fu-fit-canvas-to-layer image drawable)
  (let* (
      (layer (car (gimp-image-get-active-layer image)))
      )
    (gimp-image-undo-group-start image)
    (gimp-image-resize image (car (gimp-drawable-width layer))
                             (car (gimp-drawable-height layer))
                             (- (car (gimp-drawable-offsets layer)))
                             (- (cadr (gimp-drawable-offsets layer)))
                             )
    (gimp-image-undo-group-end image)
    )
  (gimp-displays-flush)
  )

(script-fu-register "script-fu-fit-canvas-to-layer"
 "<Image>/FX-Foundry/Toolbox/_Fit Canvas to Layer"
 _"Resize the canvas to the boundaries of the active layer"
 "Saul Goode"
 "Saul Goode"
 "1/1/2007"
 ""
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 )

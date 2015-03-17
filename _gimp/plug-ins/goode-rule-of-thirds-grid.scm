;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Rule of Thirds Grid script  for GIMP 2.4
; Copyright (C) 2007 Saul Goode
;
; Tags: tool, preset grid
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


(define (script-fu-thirds-rule-grid image drawable color line-width image-derived?)

  (let* ((width (if (= image-derived? TRUE)
                       (car (gimp-image-width image))
                       (car (gimp-drawable-width drawable))))
         (height (if (= image-derived? TRUE)
                        (car (gimp-image-height image))
                        (car (gimp-drawable-height drawable))))
         (offset-x (if (= image-derived? TRUE)
                          0
                          (car  (gimp-drawable-offsets drawable))))
         (offset-y (if (= image-derived? TRUE)
                          0
                          (cadr (gimp-drawable-offsets drawable))))
         (w1/3 (trunc (- (+ (/ width        3) 0.5) (/ line-width 2))))
         (h1/3 (trunc (- (+ (/ height       3) 0.5) (/ line-width 2))))
         (w2/3 (trunc (- (+ (/ (* width 2)  3) 0.5) (/ line-width 2))))
         (h2/3 (trunc (- (+ (/ (* height 2) 3) 0.5) (/ line-width 2))))
         (orig-color (car (gimp-context-get-foreground)))
         (orig-select)
         (layer)
         )
    (gimp-image-undo-group-start image)
    (gimp-context-set-foreground color)
    (set! orig-select (car (gimp-selection-save image)))
    (set! layer (car (gimp-layer-new image width height
                                     (car (gimp-drawable-type-with-alpha drawable))
                                     "Grid"
                                     100
                                     NORMAL)))
    (gimp-drawable-fill layer TRANSPARENT-FILL)
    (gimp-image-add-layer image layer -1)
    (gimp-selection-none image)
    (gimp-rect-select image w1/3 0    line-width height     CHANNEL-OP-ADD FALSE 0)
    (gimp-rect-select image 0    h1/3 width      line-width CHANNEL-OP-ADD FALSE 0)
    (gimp-rect-select image w2/3 0    line-width height     CHANNEL-OP-ADD FALSE 0)
    (gimp-rect-select image 0    h2/3 width      line-width CHANNEL-OP-ADD FALSE 0)
    (gimp-edit-fill layer FOREGROUND-FILL)
    (gimp-layer-translate layer offset-x offset-y)
    (gimp-selection-load orig-select)
    (gimp-image-remove-channel image orig-select)
    (gimp-context-set-foreground orig-color)
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
    )
  )


(script-fu-register "script-fu-thirds-rule-grid"
                    _"Rule Of Thirds Grid"
                    _"Generate a new layer with a Rule Of Thirds grid"
                    "Saul Goode"
                    "Saul Goode"
                    "2007/11/1"
                    "RGB* GRAY*"
                    SF-IMAGE       "Image"           0
                    SF-DRAWABLE    "Drawable"        0
                    SF-COLOR       "Ruler Color"     '(0 0 0)
                    SF-ADJUSTMENT  "Ruler Size"      '(2 1 50 1 10 0 1)
                    SF-TOGGLE      "Image based?"    FALSE
                    )

(script-fu-menu-register "script-fu-thirds-rule-grid"
             "<Image>/FX-Foundry/Toolbox/Grids/")

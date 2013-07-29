;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Mirror layer script  for GIMP 2.4
; Created by Saul Goode
;
; Tags: layer, mirror
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



(define (script-fu-mirror-dup image layer iterations horizontal vertical workcopy)
  (let* (
    (work-image 0)
    (new-layer 0)
    (orig-width 0)
    (orig-height 0)
    )
    (if (= workcopy TRUE)
      (begin
        (set! work-image (car (gimp-image-duplicate image)))
        (gimp-image-undo-disable work-image)
        (gimp-display-new work-image)
        )
      (begin
        (set! work-image image)
        (gimp-image-undo-group-start work-image)
        )
      )
    (gimp-selection-none work-image)
    (while (> iterations 0)
      (set! layer (car (gimp-image-get-active-layer work-image)))
      (if (> (car (gimp-image-get-layers work-image)) 1)
        (set! layer (car (gimp-image-merge-visible-layers work-image EXPAND-AS-NECESSARY)))
        )
      (if (= horizontal TRUE)
        (begin
          (set! new-layer (car (gimp-layer-copy layer 1)))
          (gimp-image-add-layer work-image new-layer -1)
          (set! orig-width (car (gimp-drawable-width new-layer)))
          (set! orig-height (car (gimp-drawable-height new-layer)))
          (gimp-layer-resize
              new-layer
              (* 2 orig-width)
                  orig-height
                  0
                  0
                  )
          (set! new-layer (car (gimp-drawable-transform-flip-simple new-layer
              ORIENTATION-HORIZONTAL
              TRUE
              orig-width
              0))
            )
          (gimp-image-resize-to-layers work-image)
          (if (> (car (gimp-image-get-layers work-image)) 1)
            (set! layer (car (gimp-image-merge-visible-layers work-image EXPAND-AS-NECESSARY)))
            )
          )
        )
      (if (= vertical TRUE)
        (begin
          (set! new-layer (car (gimp-layer-copy layer 1)))
          (gimp-image-add-layer work-image new-layer -1)
          (set! orig-width (car (gimp-drawable-width new-layer)))
          (set! orig-height (car (gimp-drawable-height new-layer)))
          (gimp-layer-resize
              new-layer
              orig-width
              (* 2 orig-height)
              0
              0
              )
          (set! new-layer (car (gimp-drawable-transform-flip-simple new-layer
              ORIENTATION-VERTICAL
              TRUE
              orig-height
              0))
            )
          (gimp-image-resize-to-layers work-image)
          )
        )
      (set! iterations (- iterations 1))
      )
    (gimp-selection-none work-image)
    (if (> (car (gimp-image-get-layers work-image)) 1)
      (set! layer (car (gimp-image-merge-visible-layers work-image EXPAND-AS-NECESSARY)))
      )
    (gimp-displays-flush)
    (if (= workcopy TRUE)
      (begin
        (gimp-image-undo-enable work-image)
        (gimp-image-clean-all work-image)
        )
      (gimp-image-undo-group-end image)
      )
    )
  )

(script-fu-register "script-fu-mirror-dup"
 "<Image>/FX-Foundry/Layer Effects/Apply Mirrors..."
 "Duplicates the image with mirror images"
 "Saul Goode"
 "Saul Goode"
 "4/17/2006"
 ""
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 SF-ADJUSTMENT "Iterations (Image doubles each time)" '( 1 0 10 1 1 0 1 )
 SF-TOGGLE "Horizontal direction" TRUE
 SF-TOGGLE "Vertical direction" FALSE
 SF-TOGGLE "Work on copy" TRUE
 )

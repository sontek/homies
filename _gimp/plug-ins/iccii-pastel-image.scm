;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Pastel image script  for GIMP 2.4
; Copyright (C) 2001 Iccii <iccii@hotmail.com>
; Modified for GIMP 2.x by Tim Jacobs
;
; Tags: artistic
;
; Author statement:
;
; This script is based on pastel-windows100.scm
; --------------------------------------------------------------------
;     Reference Book
; Windows100% Magazine October, 2001
;   Tamagorou's Photograph touching up class No.29
;     theme 1 -- Create the Pastel image
; --------------------------------------------------------------------
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; version 0.1  by Iccii 2001/10/19 <iccii@hotmail.com>
;     - Initial relase
; version 0.2 by Tim Jacobs 2004/04/15
;     - Modified to work for GIMP 2.x
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


(define (script-fu-pastel-image
            img
            drawable
            detail
            length
            amount
            angle
            canvas?
    )


  (gimp-image-undo-group-start img)

  (let* (
     (old-selection (car (gimp-selection-save img)))
     (layer-copy0 (car (gimp-layer-copy drawable TRUE)))
         (dummy (gimp-image-add-layer img layer-copy0 -1))
     (dummy (if (< 0 (car (gimp-layer-get-mask layer-copy0)))
                  (gimp-layer-remove-mask layer-copy0 MASK-DISCARD)))
     (layer-copy1 (car (gimp-layer-copy layer-copy0 TRUE)))
     (length (if (= length 1) 0 length))
     (dummy (begin
              (plug-in-mblur TRUE img layer-copy0 0 length angle 0 0 )
              (plug-in-mblur TRUE img layer-copy0 0 length (+ angle 180) 0 0 ) ))
     (layer-copy2 (car (gimp-layer-copy layer-copy0 TRUE)))
     (marged-layer 0)
     (final-layer 0)
    )

    (gimp-image-add-layer img layer-copy2 -1)
    (gimp-image-add-layer img layer-copy1 -1)

    ;;
    (plug-in-gauss-iir TRUE img layer-copy1 (- 16 detail) TRUE TRUE)
    (plug-in-edge TRUE img layer-copy1 10.0 1 0)
    (gimp-layer-set-mode layer-copy1 DIVIDE-MODE)
    (set! marged-layer (car (gimp-image-merge-down img layer-copy1 EXPAND-AS-NECESSARY)))
    (gimp-layer-set-mode marged-layer VALUE-MODE)

    ;; Add the canvas if asked to
    (if (equal? canvas? TRUE)
        (plug-in-apply-canvas TRUE img marged-layer 0 5))
    (plug-in-unsharp-mask TRUE img layer-copy0 (+ 1 (/ length 5)) amount 0)
    (set! final-layer (car (gimp-image-merge-down img marged-layer EXPAND-AS-NECESSARY)))
    (gimp-selection-load old-selection)
    (gimp-edit-copy final-layer)
    (gimp-image-remove-layer img final-layer)
    (gimp-floating-sel-anchor (car (gimp-edit-paste drawable 0)))
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)

    ;; Clean up
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
  "script-fu-pastel-image"
  "<Image>/FX-Foundry/Artistic/Pastel Sketch..."
  "Create the Pastel image"
  "Iccii <iccii@hotmail.com>"
  "Iccii"
  "2001, Oct"
  "RGB*"
  SF-IMAGE      "Image"             0
  SF-DRAWABLE   "Drawable"       0
  SF-ADJUSTMENT "Detail Level"   '(12.0 0 15.0 0.1 0.5 1 1)
  SF-ADJUSTMENT "Sketch Length" '(10 0 32 1 1 0 1)
  SF-ADJUSTMENT "Sketch Amount" '(1.0 0 5.0 0.1 0.5 1 1)
  SF-ADJUSTMENT "Angle"          '(45 0 180 1 15 0 0)
  SF-TOGGLE     "Add the canvas texture" FALSE
 )

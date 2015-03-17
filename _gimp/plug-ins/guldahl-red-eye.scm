;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Red Eye Correction script  for GIMP 2.4
; Copyright (C) 2002 Martin Guldahl <mguldahl@xmission.com>
;
; Tags: photo, red eye
;
; Author statement:
;
; Removes red eye
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; 0.95 - initial release
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


(define (script-fu-red-eye image
               drawable
               radius
               threshold
               red
               green
               blue)


  (let* ((select-bounds (gimp-drawable-mask-bounds drawable))
     (x1 (cadr select-bounds))
         (y1 (caddr select-bounds))
         (x2 (cadr (cddr select-bounds)))
         (y2 (caddr (cddr select-bounds)))
     (red-component 0)
     (green-component 1)
     (blue-component 2)
     )

  (gimp-image-undo-group-start image)

; Select and view the green channel only as it has the best contrast
;  between the iris and pupil.
;    - Deselect the Red and Blue channels

  (gimp-image-set-component-active image red-component FALSE)
  (gimp-image-set-component-active image blue-component FALSE)

; Use the fuzzy select tool (select contiguous regions) to select the pupils
  (gimp-fuzzy-select
   drawable
   x1 y1
   threshold 0 TRUE TRUE radius FALSE
   )

; Reselect Red and Blue channels
  (gimp-image-set-component-active image red-component TRUE)
  (gimp-image-set-component-active image blue-component TRUE)

; Use the channel mixer to make selected area monochrome
  (plug-in-colors-channel-mixer
   ;;run_mode
   1
   image
   drawable
   ;;monochrome
   1
   (/ red 100) (/ green 100) (/ blue 100) 0 0 0  0 0 0
   )

  (gimp-selection-clear image)
  (gimp-image-undo-group-end image)
  (gimp-displays-flush)
  )
  )

(define (script-fu-red-eye-no-cm image
               drawable
               radius
               threshold
               )


  (let* ((select-bounds (gimp-drawable-mask-bounds drawable))
     (x1 (cadr select-bounds))
         (y1 (caddr select-bounds))
         (x2 (cadr (cddr select-bounds)))
         (y2 (caddr (cddr select-bounds)))
     (red-component 0)
     (green-component 1)
     (blue-component 2)
     )

  (gimp-image-undo-group-start image)

; Select and view the green channel only as it has the best contrast
;  between the iris and pupil.
;    - Deselect the Red and Blue channels

  (gimp-image-set-component-active image red-component FALSE)
  (gimp-image-set-component-active image blue-component FALSE)

; Use the fuzzy select tool (select contiguous regions) to select the pupils
  (gimp-fuzzy-select
   drawable
   x1 y1
   threshold 0 TRUE TRUE radius FALSE
   )

; Reselect Red and Blue channels
  (gimp-image-set-component-active image red-component TRUE)
  (gimp-image-set-component-active image blue-component TRUE)

; Now desaturate the Red channel
  (gimp-image-set-component-active image red-component TRUE)
  (gimp-image-set-component-active image green-component FALSE)
  (gimp-image-set-component-active image blue-component FALSE)
  (gimp-desaturate drawable)
  (gimp-image-set-component-active image red-component TRUE)
  (gimp-image-set-component-active image green-component TRUE)
  (gimp-image-set-component-active image blue-component TRUE)

  (gimp-selection-clear image)
  (gimp-image-undo-group-end image)
  (gimp-displays-flush)
  )
  )

(script-fu-register "script-fu-red-eye"
            _"<Image>/FX-Foundry/Photo/Enhancement/Red Eye..."
            "Removes red eye; given selection is seed.\nNeeds the channel-mixer plug-in."
            "Martin Guldahl <mguldahl@xmission.com>"
            "Martin Guldahl"
            "2002/10/05"
            "*"
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Drawable" 0
            SF-ADJUSTMENT _"Radius" '(8 0 100 .2 1 1 1)
            SF-ADJUSTMENT _"Threshold" '(40 0 255 1 1 1 1)
            SF-ADJUSTMENT _"Red" '(10 0 100 1 1 1 1)
            SF-ADJUSTMENT _"Green" '(60 0 100 1 1 1 1)
            SF-ADJUSTMENT _"Blue" '(30 0 100 1 1 1 1)
            )

(script-fu-register "script-fu-red-eye-no-cm"
            _"<Image>/FX-Foundry/Photo/Enhancement/Red Eye Desaturate..."
            "Removes red eye; given selection is seed."
            "Martin Guldahl <mguldahl@xmission.com>"
            "Martin Guldahl"
            "2002/10/05"
            "*"
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Drawable" 0
            SF-ADJUSTMENT _"Radius" '(8 0 100 .2 1 1 1)
            SF-ADJUSTMENT _"Threshold" '(40 0 255 1 1 1 1)
            )

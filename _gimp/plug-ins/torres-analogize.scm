;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Analogize script  for GIMP 2.4
; Copyright (C) 2005 Ismael Valladolid Torres <ivalladt@punkass.com>
;
; Tags: photo, old
;
; Author statement:
;; A script-fu for the GIMP that makes any picture look as if it had
;; been taken using an old analog camera. Exaggerates contrast and
;; saturation and creates a bright and a dark overlay randomly
;; placed. Think of it as kind of a Lomo Kompakt or Kodak instantmatic
;; faking effect. However it still can't make anything to emulate the
;; peculiar chromatism usually achieved using the real thing.
;;
;; Check http://analogize.berlios.de/ for more information.
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

(define (script-fu-analogize img
                 drawable
                 contrast
                 saturation
                 bright-opacity
                 shadow-opacity
                 duplicate-shadow
                 flatten
                 copy)
 (let* (
        (image 0)
        (layer 0)
        (image-width 0)
        (image-height 0)
        (half-image-width 0)
        (half-image-height 0)
        (center-x 0)
        (center-y 0)
        (bright-layer 0)
        (shadow-layer 0)
        (shadow-layer2 0)
        (width-factor 0)
        (height-factor 0)
       )

  (set! image (if (= copy TRUE)
          (car (gimp-image-duplicate img))
          img))

  (gimp-image-undo-group-start image)

  (set! layer (car (gimp-image-flatten image)))

  (set! image-width (car (gimp-image-width image)))
  (set! image-height (car (gimp-image-height image)))

  (set! half-image-width (/ image-width 2))
  (set! half-image-height (/ image-height 2))

  (set! width-factor (/ (- 85 (rand 170)) 100))
  (set! height-factor (/ (- 85 (rand 170)) 100))

  (set! center-x (+ half-image-width (* half-image-width width-factor)))
  (set! center-y (+ half-image-height (* half-image-height height-factor)))

  (gimp-brightness-contrast layer 0 contrast)
  (gimp-hue-saturation layer 0 0 0 saturation)

  (set! bright-layer (car (gimp-layer-new image
                      image-width
                      image-height
                      1 "Brillo" bright-opacity 5)))

  (gimp-image-add-layer image bright-layer 0)
  (gimp-edit-clear bright-layer)
  (gimp-palette-set-foreground '(255 255 255))

  (gimp-edit-blend bright-layer 2 0 2 100 0 0 FALSE FALSE 0 0 TRUE
           center-x center-y
           (+ half-image-width center-x) 0)

  (set! shadow-layer (car (gimp-layer-new image
                      image-width
                      image-height
                      1 "Sombra" shadow-opacity 5)))

  (gimp-image-add-layer image shadow-layer 0)
  (gimp-edit-clear shadow-layer)
  (gimp-palette-set-foreground '(0 0 0))

  (if (= (rand 2) 0)
      (begin
    (gimp-edit-blend shadow-layer 2 0 0 100 0 0 FALSE FALSE 0 0 TRUE
             0 0
             center-x center-y)

    (gimp-edit-blend shadow-layer 2 0 0 100 0 0 FALSE FALSE 0 0 TRUE
             image-width image-height
             center-x center-y))
      (begin
    (gimp-edit-blend shadow-layer 2 0 0 100 0 0 FALSE FALSE 0 0 TRUE
             image-width 0
             center-x center-y)

    (gimp-edit-blend shadow-layer 2 0 0 100 0 0 FALSE FALSE 0 0 TRUE
             0 image-height
             center-x center-y)))

  (cond ((= duplicate-shadow TRUE)
     (set! shadow-layer2 (car (gimp-layer-copy shadow-layer 0)))
     (gimp-image-add-layer image shadow-layer2 0)))

  (cond ((= flatten TRUE)
     (gimp-image-flatten image)))

  (cond ((= copy TRUE)
     (gimp-display-new image)))

  (gimp-image-undo-group-end image)
  (gimp-displays-flush)
  )
)

(script-fu-register "script-fu-analogize"
            "Analogize..."
            "A simple analog camera faking effect"
            "Ismael Valladolid Torres <ivalladt@punkass.com>"
            "Ismael Valladolid Torres"
            "2005"
            "RGB*"
            SF-IMAGE "The image" 0
            SF-DRAWABLE "The layer" 0
            SF-ADJUSTMENT "Contrast" '(20 0 60 1 5 0 0)
            SF-ADJUSTMENT "Saturation" '(20 0 60 1 5 0 0)
            SF-ADJUSTMENT "Bright layer opacity" '(80 0 100 1 10 0 0)
            SF-ADJUSTMENT "Shadow layer opacity" '(100 0 100 1 10 0 0)
            SF-TOGGLE "Duplicate the shadow layer" TRUE
            SF-TOGGLE "Flatten image after processing" TRUE
            SF-TOGGLE "Work on copy" TRUE)

(script-fu-menu-register "script-fu-analogize"
             "<Image>/FX-Foundry/Artistic")

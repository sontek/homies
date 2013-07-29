;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Wonderful script  for GIMP 2.4
; Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
;
; Tags: photo, effect
;
; Author statement:
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;
; Updated for GIMP 2.4 by Paul Sherman
; --------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-grumbel-wonderful inImage inDrawable blurfactor brightness contrast flatten)
  (gimp-image-undo-group-start inImage) ;; d�but d'historique d'annulation

  (let ((new-layer (car (gimp-layer-copy inDrawable 1))))
    (gimp-image-add-layer inImage  new-layer 0)
    (plug-in-gauss-iir 1 inImage new-layer blurfactor 1 1)
;; Replace this with some level stuff
    (gimp-brightness-contrast new-layer brightness contrast)

    (let ((layer-mask (car (gimp-layer-create-mask inDrawable WHITE-MASK))))
      (gimp-layer-add-mask new-layer layer-mask)
      (gimp-edit-copy new-layer)
      (gimp-floating-sel-anchor (car (gimp-edit-paste layer-mask 0)))
      (gimp-layer-set-mode new-layer ADDITION)))

  (if (= flatten TRUE)
      (gimp-image-flatten inImage))


  (gimp-displays-flush)

  (gimp-image-undo-group-end inImage)) ;; fin d'historique d'annulation

(script-fu-register "script-fu-grumbel-wonderful"
            "<Image>/FX-Foundry/Photo/Effects/Make wonderful..."
            "Creates a new tuxracer level"
            "Ingo Ruhnke"
            "1999, Ingo Ruhnke"
            "2000"
            "RGB RGBA"
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Drawable" 0
            SF-ADJUSTMENT "Flou" '(35 0 5600 1 100 0 1)
            SF-ADJUSTMENT "Luminosite" '(0 -127 127 1 10 0 1)
            SF-ADJUSTMENT "Contraste" '(0 -127 127 1 10 0 1)
            SF-TOGGLE "Aplatir l'image" FALSE)

;; EOF ;;

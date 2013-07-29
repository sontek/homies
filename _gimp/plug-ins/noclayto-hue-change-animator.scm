;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Hue Change Animator script  for GIMP 2.4
; Original author: noclayto <www.gimptalk.com>
;
; Tags: animation
;
; Author statement:
;
;;  Will cycle through the HUE-MODE by a step.
;; This script is mainly for learning. Use at your own risk.
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



(define (script-fu-hue-change-animator img drawable step)

  (let* (
     (counter step)
     (new-layer drawable)
     )

    (gimp-image-undo-group-start img)
    ;;(gimp-image-raise-layer-to-top img drawable) ;;error???


    (while (< counter 180)
       (set! new-layer (car (gimp-layer-copy drawable TRUE)))
       (gimp-image-add-layer img new-layer -1)
       (gimp-hue-saturation new-layer 0 counter 0 0)
       (set! counter (+ counter step))
       )

    (set! counter (- counter 360))

    (while (< counter 0)
       (set! new-layer (car (gimp-layer-copy drawable TRUE)))
       (gimp-image-add-layer img new-layer -1)
       (gimp-hue-saturation new-layer 0 counter 0 0)
       (set! counter (+ counter step))
       )




    (gimp-image-undo-group-end img)
    (gimp-displays-flush)))

(script-fu-register "script-fu-hue-change-animator"
            _"<Image>/FX-Foundry/Animation/Hue Changer ..."
            ""
            "noclayto"
            "noclayto"
            "July 2005"
            ""
                    SF-IMAGE       "Image"    0
            SF-DRAWABLE    "Drawable" 0
            SF-ADJUSTMENT _"Color Step"  '(45 1 360 1 10 0 1)
            )





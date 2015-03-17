;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Fade Animator script  for GIMP 2.4
; Original author: noclayto <www.gimptalk.com>
;
; Tags: animation
;
; Author statement:
;
;;  Will fade a layer to transparent.
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

(define (script-fu-fade-animator img drawable step)

  (let* (
     (counter step)
     (new-layer 0)
     (fade-to-layer 0)
     (num-layers  (car  (gimp-image-get-layers img)))
     (num num-layers)
     (num-temp (- num 1))
     (layers (cadr (gimp-image-get-layers img)))
     (new-img (car (gimp-image-new
            (car (gimp-image-width img))
            (car (gimp-image-height img))
            RGB)))


     )

    ;;(aref layers (- num 1)))
    (gimp-context-push)
    (gimp-image-undo-group-start new-img)


    (while (> num 0)
       (set! new-layer (car (gimp-layer-new-from-drawable (aref layers (- num 1)) new-img)))
       (gimp-image-add-layer new-img new-layer -1)

       (set! new-layer (car (gimp-layer-new-from-drawable (aref layers (- num 1)) new-img)))
       (gimp-image-add-layer new-img new-layer -1)

       (if (< num-temp 1) (set! num-temp num-layers ))
       ;;add layer above.  then add level adjusted layer.  then merge down
       (while (< counter 255)
          (set! new-layer (car (gimp-layer-new-from-drawable (aref layers (- num-temp 1)) new-img)))
          (gimp-image-add-layer new-img new-layer -1)

          (set! new-layer (car (gimp-layer-new-from-drawable (aref layers (- num 1)) new-img)))
          (gimp-image-add-layer new-img new-layer -1)

          (gimp-levels new-layer 4 0 255 1 0 (- 255 counter))
          (gimp-image-merge-down new-img new-layer 0)
          (set! counter (+ counter step))
          )
       (set! counter step)
       (set! num (- num 1))
       (set! num-temp (- num-temp 1))
       )


    (gimp-image-undo-group-end new-img)
    (gimp-display-new new-img)
    (gimp-context-pop)
    (gimp-displays-flush)))

(script-fu-register "script-fu-fade-animator"
            _"<Image>/FX-Foundry/Animation/Fade..."
            ""
            "noclayto"
            "noclayto"
            "July 2005"
            "RGB"
                    SF-IMAGE       "Image"    0
            SF-DRAWABLE    "Drawable" 0
            SF-ADJUSTMENT _"Color Step"  '(51 1 127 1 10 0 1)
            )





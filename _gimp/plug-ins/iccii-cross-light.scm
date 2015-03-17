;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Cross light script  for GIMP 2.4
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
; version 0.1  by Iccii 2001/07/22
;     - Initial relase
; version 0.2  by Iccii 2001/08/09
;     - Add the Start Angle and the Number of Lighting options
; version 0.3  by Tim Jacobs 2004/04/13
;     - Modified for GIMP 2.x
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



(define (script-fu-cross-light
            img
            drawable
            length
            angle
            number
            threshold
    )
  (let* (
     (modulo fmod)
     (count 1)
     (target-layer (car (gimp-layer-copy drawable TRUE)))
     (layer-mask (car (gimp-layer-create-mask target-layer ADD-WHITE-MASK)))
     (merged-layer 0)
        )

    (gimp-image-undo-group-start img)

;; Create the highlight mask and copy into buffer
    (let* (
       (tmp-layer (car (gimp-layer-copy drawable TRUE)))
          )
      (gimp-image-add-layer img tmp-layer -1)
      (gimp-desaturate tmp-layer)
      (gimp-threshold tmp-layer threshold 255)
      (gimp-edit-copy tmp-layer)
      (gimp-image-remove-layer img tmp-layer)
    )

;; Create a layer that has just the highlights
    (gimp-image-add-layer img target-layer -1)
    (gimp-layer-add-mask target-layer layer-mask)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-mask 0)))
    (gimp-layer-remove-mask target-layer MASK-APPLY)

;; Copy the highlights layer, and motion blur for each "light"
    (while (<= count number)
      (let* (
             (layer-copy (car (gimp-layer-copy target-layer TRUE)))
             (degree (modulo (+ (* count (/ 360 number)) angle) 360))
            )
        (gimp-image-add-layer img layer-copy -1)
        (plug-in-mblur 1 img layer-copy 0 length degree 0 0)
        (if (> count 1)
            (set! merged-layer (car (gimp-image-merge-down img layer-copy
                                                       EXPAND-AS-NECESSARY)))
            ()
        )
        (set! count (+ count 1))
      ) ; end of let*
    ) ; end of while

;; Finishing touches and clean up
    (gimp-layer-set-opacity merged-layer 80)
    (gimp-layer-set-mode merged-layer SCREEN-MODE)
    (gimp-image-remove-layer img target-layer)

    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
  "script-fu-cross-light"
  "<Image>/FX-Foundry/Photo/Effects/Cross Light..."
  "Cross light effect"
  "Iccii <iccii@hotmail.com>"
  "Iccii"
  "2001, Aug"
  "RGB*"
  SF-IMAGE      "Image"            0
  SF-DRAWABLE   "Drawable"        0
  SF-ADJUSTMENT _"Light Length"        '(40 1 255 1 10 0 0)
  SF-ADJUSTMENT _"Start Angle"        '(30 0 360 1 10 0 0)
  SF-ADJUSTMENT "Number of Lights"    '(4 1 16 1 2 0 1)
  SF-ADJUSTMENT _"Threshold (Bigger 1<-->255 Smaller)"  '(223 1 255 1 10 0 0)
)

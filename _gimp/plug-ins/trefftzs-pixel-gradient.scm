;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; pixel gradient script  for GIMP 2.4
; Original author: Jeff Trefftzs <trefftzs@tcsn.net>
;
; Tags: effect
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

;;; Define the function:

(define (script-fu-pixelgradient inImage inLayer minsize maxsize nsteps)
  ;; If there isn't already a selection, select the whole thing
 (let* (
        (noselection 0)
       )
  (if (car (gimp-selection-bounds inImage))
      (begin
        (set! noselection FALSE)
      )
      (begin
       (gimp-selection-all inImage)
       (set! noselection TRUE)
      )
   )

  (let* (
     (selchannel (car (gimp-selection-save inImage)))
     (selstuff  (gimp-selection-bounds inImage))
     (width
      (cond ((car selstuff)
         (- (nth 3 selstuff) (nth 1 selstuff)))
        (t (car (gimp-image-width inImage)))))
     (height
      (cond ((car selstuff)
         (- (nth 4 selstuff) (nth 2 selstuff)))
        (t (car (gimp-image-height inImage)))))
     (x0
      (cond ((car selstuff)
         (nth 1 selstuff))
        (t 0)))
     (y0
      (cond ((car selstuff)
         (nth 2 selstuff))
        (t 0)))
     (x1 width)
     (y1 height)
     (stepwidth (/ width nsteps))
     (pixstep (/ (- maxsize minsize) nsteps))
     (startx x0)
     (startsize minsize)
     )

  (gimp-image-undo-group-start inImage)

  ;; Step across the selection (or image), pixelizing as we go

  (while (< startx x1)
     (begin
       (gimp-selection-load selchannel)
       (gimp-rect-select inImage startx y0 stepwidth height
                 CHANNEL-OP-INTERSECT
                 FALSE
                 0)

       (plug-in-pixelize TRUE inImage inLayer startsize)
       (set! startx (+ startx stepwidth))
       (set! startsize (+ startsize pixstep))
      )
     )

  (if (equal? TRUE noselection)
      (gimp-selection-none inImage)
      (gimp-selection-load selchannel)
      )
  )
  (gimp-image-set-active-layer inImage inLayer)
  (gimp-image-undo-group-end inImage)
  (gimp-displays-flush)
  )
)

(script-fu-register
 "script-fu-pixelgradient"
 _"<Image>/FX-Foundry/Selection Effects/Pixel Gradient"
 "Pixelizes a selection (or layer) from left to right with increasing pixel sizes."
 "Jeff Trefftzs"
 "Copyright 2003, Jeff Trefftzs"
 "November 17, 2003"
 "RGB* GRAY* INDEXED*"
 SF-IMAGE "The Image" 0
 SF-DRAWABLE "The Layer" 0
 SF-ADJUSTMENT "Minimum Pixel Size" '(5 1 256 1 5 0 1)
 SF-ADJUSTMENT "Maximum Pixel Size" '(64 1 256 1 5 0 1)
 SF-ADJUSTMENT "Number of Steps"  '(5 1 256 1 5 0 1)
)

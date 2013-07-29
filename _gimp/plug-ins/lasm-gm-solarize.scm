;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; LASM Solarize script  for GIMP 2.4
; Original author: lasm <lasm@rocketmail.com>
;;;  http://www.godsimmediatecontact.com
;;;  http://www.godsdirectcontact.org
;;;  http://www.raindesigninc.com
;
; Tags: color
;
; Author statement:
;
;;; Welcome to the Grandmother's Solarize
;;; Dedication - to my mother (1917-2002) in loving memory
;;;
;;; Grandmother's Solarize::
;;; Another quality script brought to you by  the Grandmother Coffee House production.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; version 1.0  by Lasm 2002/11/19 <lasm@rocketmail.com>
;    - Initial relase
; 11/01/2007 - fixed for 2.4 and inclusion in FX-foundry  by Alexia Death
;
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


(define (script-fu-gm-solarize img inLayer intens thres flat?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Helper function to create a new layer
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (copylayer layer layername)
 (let* ((new (car(gimp-layer-copy layer 1)))) ; Add an alpha channel
  (gimp-drawable-set-name new layername)
  new
 )
)

  (let*
      (
    (width (car (gimp-drawable-width inLayer)))
        (height (car (gimp-drawable-height inLayer)))
    (invert-layer (copylayer inLayer (string-append
                                                "GM Solarize "
                                                (number->string thres 10)
                                                )))
       )
  (gimp-image-undo-group-start img)

;; Real work goes in here

   (gimp-image-add-layer img invert-layer -1)
   (gimp-levels invert-layer HISTOGRAM-VALUE 0 255 1.0 255 (- 255 thres))

   (gimp-layer-set-mode invert-layer DARKEN-ONLY-MODE)               ; Darken Layer Mode
   (gimp-layer-set-opacity invert-layer intens)

   (if (eqv? flat? TRUE)
     (gimp-drawable-set-name (car (gimp-image-merge-down img invert-layer EXPAND-AS-NECESSARY))
                     (string-append
                       "GM Solarize "
                        (number->string intens 10)
                        " Intensity "
                       (number->string thres 10)
                       " threshold "
                       )))

  (gimp-image-undo-group-end img)
  (gimp-displays-flush)


  )
)


(script-fu-register
 "script-fu-gm-solarize"
 _"<Image>/FX-Foundry/Color/Lasm's Solarize"
 "Solarize effect. This works on any RGB image."
 "lasm"
 "Copyright 2005, lasm"
 "November 19, 2005"
 "RGB*"
 SF-IMAGE        "The Image"        0
 SF-DRAWABLE    "The Layer"        0
 SF-ADJUSTMENT    "Intensity"        '(100.0 0 100.0 1.0 0 1 0)
 SF-ADJUSTMENT  _"Threshold"    '(255 0 255 1 10 0 0)
 SF-TOGGLE        _"Flatten"         TRUE
)

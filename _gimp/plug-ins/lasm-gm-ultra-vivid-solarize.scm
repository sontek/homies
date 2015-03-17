;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; LASM ultra vivid solarize script  for GIMP 2.4
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
;;; Lasm's famous fancy solarize script
;;; Dedication - to my mother (1917-2002) in loving memory
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; version 1.0  by Lasm 2005/12/01 <lasm@rocketmail.com>
;     - Initial release
; version 2.0  by Lasm 2005/12/11 <lasm@rocketmail.com>
;     - added Automatic option with 2 effects
; 11/01/2007 - fixed for Gimp 2.4 by Alexia Death
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Grandmother's Ultra Vivid Solarize::
;;; Requires:
;;;
;;; Another quality script brought to you by the Grandmother Coffee House production.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes on Options
;;; 1. Automatic (Ultra)-Vivid gives vivid and highly saturated color solarization
;;; 2. Automatic Vivid gives colorful (purple) solarization
;;; 3. With Automatic turned off, the (Ultra) and Vivid solarization
;;;    needs manual input adjustment via the sliders.
;;; 4. With Automatic turned off, Vivid at 127 is equivalent to
;;;    Normal Solarize (slight difference due to curve smoothing)
;;; 5. With Automatic turned on, (Ultra)-Vivid is close to
;;;    Automatic off, Vivid at 255, except there is richer color enhancement.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  (Ultra)-Vivid-Solarize
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (script-fu-gm-ultra-vivid-solarize img inLayer auto? ultra? intense)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Helper functions to return curves array
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define (get-gm-color)
  (let* ((curve-value (cons-array 6 'byte)))
   (aset curve-value 0 0)
   (aset curve-value 1 0)
   (aset curve-value 2 127)
   (aset curve-value 3 255)
   (aset curve-value 4 255)
   (aset curve-value 5 0)
   curve-value     ; return the curve
   )
 )
 (define (get-gm-solarize s-type curvestr)
  (let* ((curve-value (cons-array 4 'byte)))
   (aset curve-value 0
         (if (= s-type 0)
           0
           curvestr))
   (aset curve-value 1 0)
   (aset curve-value 2 255)
   (aset curve-value 3
         (if (= s-type 0)
           curvestr
           255))
   curve-value     ; return the curve
   )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Main function
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (let*
      ()

  (gimp-image-undo-group-start img)

;; Real work goes in here

  (gimp-curves-spline inLayer HISTOGRAM-RED 6 (get-gm-color))
  (gimp-curves-spline inLayer HISTOGRAM-GREEN 6 (get-gm-color))
  (gimp-curves-spline inLayer HISTOGRAM-BLUE 6 (get-gm-color))

  (gimp-curves-spline inLayer HISTOGRAM-VALUE 4 (get-gm-solarize
  (if (eqv? auto? TRUE)
    (begin
     (set! intense 127)
     0)
     (if (eqv? ultra? TRUE)
      1
      0))
                                        intense))

  (if (eqv? auto? TRUE)
    (begin
    (if (eqv? ultra? TRUE)
      (begin
      (plug-in-color-enhance RUN-NONINTERACTIVE img inLayer)
      (gimp-levels-stretch inLayer))
      (gimp-equalize inLayer FALSE)
      )
    ))

  (gimp-drawable-set-name inLayer (string-append
                                        "GM Solarize"
                                        (if (eqv? auto? TRUE)
                                          " Automatic" "" )
                                        (if (eqv? ultra? TRUE)
                                          " Ultra-" " " )
                                        "Vivid ("
                                        (number->string intense 10)
                                        ")"
                                        ))

  (gimp-image-undo-group-end img)
  (gimp-displays-flush)

  )
)

;; Register scripts

(script-fu-register
 "script-fu-gm-ultra-vivid-solarize"
 _"<Image>/FX-Foundry/Color/Lasm's Asymmetric Ultra-Vivid Solarize"
 "Version 2.0\nLasm's fancy solarize effect. Toggle the (Ultra) Vivid button for a turbo-charged boost to the solarize effect."
 "lasm"
 "Copyright 2005, lasm"
 "December 01, 2005"
 "RGB*"
 SF-IMAGE        "The Image"            0
 SF-DRAWABLE    "The Layer"            0
 SF-TOGGLE        _"Automatic"        TRUE
 SF-TOGGLE        _"(Ultra)-Vivid"    TRUE
 SF-ADJUSTMENT  _"Intensity"        '(127 0 255 1 10 0 0)
)


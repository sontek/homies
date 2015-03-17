;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; LASM Ultravivid script  for GIMP 2.4
; Original author: lasm <lasm@rocketmail.com>
;;;  http://www.godsimmediatecontact.com
;;;  http://www.godsdirectcontact.org
;;;  http://www.raindesigninc.com
;
; Tags: color
;
; Author statement:
;
; Welcome to the Line Art Coffee House
; This Ultra Vivid Lab script is for coffee-connisseurs only
; If it doesn't work for your images, perhaps you prefer the Bubble Tea House next door ?
; Dedication - to my mother (1917-2002)
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; version 1.0  by Lasm 2005/26/11 <lasm@rocketmail.com>
;     - Initial release
; version 1.1  by Lasm 2005/01/12 <lasm@rocketmail.com>
;     - cosmetic change of button name
; 11/01/2007 - fixed for Gimp 2.4 by Alexia Death
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Instructions on using this script
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Copy this script in the script directory.
; 2. Open up your favourite color photo in Gimp.
; 4. Look for it under Script-Fu->Lasm's FX Effects and fire away !
;
; That's all folks. Have fun with this script !
; Another Grandmother Coffee House production.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set register common information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Advanced Ultra-Vivid LAB Color function:
;
; Requires:
;   plug-in-decompose
;   plug-in-recompose
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-lab-ultra-vivid img inLayer uv? hue light sat flat?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Helper function to create layer names
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define (make-layer-name uv? Hue Light Sat)
   (define wuji1 (number->string Hue 10))
   (define wuji2 (number->string Light 10))
   (define wuji3 (number->string Sat 10))
   (string-append
      "Photo Lab "
      (if (eqv? uv? TRUE)
        " Ultra-" "" )
        "Vivid H("
        wuji1
        ") V("
        wuji2
        ") S("
        wuji3
        ")"
   )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Main function
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (let*
    (
          (uv-layer 0)
          (gray-img 0)
          (B-layer 0)
          (A-layer 0)
          (L-layer 0)
          (K-layer 0)
          (Y-layer 0)
          (M-layer 0)
          (C-layer 0)

    )
    (gimp-image-undo-group-start img)

;; Real work goes in here

    (set! uv-layer (car (gimp-layer-copy inLayer 1)))
    (gimp-image-add-layer img uv-layer -1)

    (if (eqv? uv? TRUE)
      (begin
      (set! gray-img (car (plug-in-decompose 1 img uv-layer "Lab" TRUE)))
      (set! B-layer (car (gimp-image-get-active-layer gray-img)))
      (set! A-layer (- B-layer 1))
      (set! L-layer (- A-layer 1))

      (gimp-levels-stretch B-layer)
      (gimp-levels-stretch A-layer)
      (gimp-levels-stretch L-layer))
      (begin
      (set! gray-img (car (plug-in-decompose 1 img uv-layer "CMYK" TRUE)))
      (set! K-layer (car (gimp-image-get-active-layer gray-img)))
      (set! Y-layer (- K-layer 1))
      (set! M-layer (- Y-layer 1))
      (set! C-layer (- M-layer 1))

      (gimp-levels-stretch K-layer)
      (gimp-levels-stretch Y-layer)
      (gimp-levels-stretch M-layer)
      (gimp-levels-stretch C-layer)

      ))

    (plug-in-recompose RUN-NONINTERACTIVE gray-img uv-layer)

     (gimp-drawable-set-name uv-layer (make-layer-name uv? hue light sat))
     (gimp-layer-set-mode uv-layer SOFTLIGHT-MODE)
     (plug-in-gauss-rle2 RUN-NONINTERACTIVE img uv-layer 1.2 1.2)
     (gimp-hue-saturation uv-layer 0 hue light sat)


 (if (eqv? flat? TRUE)
     (gimp-drawable-set-name (car (gimp-image-merge-down img uv-layer EXPAND-AS-NECESSARY))
                     (make-layer-name uv? hue light sat)))
;; clean up before exit

    (gimp-image-delete gray-img)

    (gimp-image-undo-group-end img)
    (gimp-displays-flush)

  )
)


(script-fu-register
 "script-fu-lab-ultra-vivid"
 _"<Image>/FX-Foundry/Color/Lasm's LAB Ultra Vivid"
 "Lasm's famous special effect for photographs. Uncheck Ultra-Vivid for a different saturation. You can use the hue saturation plugin to change colors. Caution: it runs slowly on large images."
 "lasm"
 "Copyright 2005, lasm"
 "Nov 26, 2005"
 "RGB*"
 SF-IMAGE           "The Image"       0
 SF-DRAWABLE         "The Layer"       0
 SF-TOGGLE            _"(Ultra)-Vivid"      TRUE
 SF-ADJUSTMENT      _"Hue"              '(0 -180 180 1 10 0 0)
 SF-ADJUSTMENT      _"Lightness"      '(0 -100 100 1 10 0 0)
 SF-ADJUSTMENT      _"Saturation"      '(0 -100 100 1 10 0 0)
 SF-TOGGLE            _"Flatten"          FALSE
 )

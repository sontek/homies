;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Lasm's color invert effect  script  for GIMP 2.4
; Original author: lasm <lasm@rocketmail.com>
;;;  http://www.godsimmediatecontact.com
;;;  http://www.godsdirectcontact.org
;;;  http://www.raindesigninc.com
;
; Tags: color, invert
;
; Author statement:
;
;;; Welcome to the Grandmother's Light Invert
;;; Dedication - to my mother (1917-2002) in loving memory
;;; Grandmother's Color Invert::
;;; This effect is fully reversible.
;;; Another quality script brought to you by  the Grandmother Coffee House production.
;;; Created in the Special Palindrome Day of the century 20022002
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;;; version 1.0  by Lasm 2002/02/20 <lasm@rocketmail.com>
;;;     - Initial relase
;;; version 1.1  by Lasm 2005/11/08 <lasm@rocketmail.com>
;;;     - Updated for Gimp 2.3.4
;;;     - changed file name
;;; version 1.2  by Lasm 2005/11/09 <lasm@rocketmail.com>
;;;     - rearranged code
;;; version 1.2.1  by Lasm 2005/11/17 <lasm@rocketmail.com>
;;;     - added help text
;;; version 1.3  by Lasm 2005/11/18 <lasm@rocketmail.com>
;;;     - added to Sourceforge CVS
;;;     - added Asymmetric/Solarize
;;; version 2.0  by Lasm 2005/11/20 <lasm@rocketmail.com>
;;;     - added Vivid V-Invert
;;;     - shorten fly-out menu
;;; 11/01/2007 - fixed for gimp 2.4 by Alexia Death
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


;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Color-invert
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (script-fu-gm-color-invert invert-type img inLayer)

(define (copylayer layer layername)
  (let* ((new (car(gimp-layer-copy layer 1)))) ; Add an alpha channel
  (gimp-drawable-set-name new layername)
  new
  )
)

  (let*
      ((invert-layer (copylayer inLayer "Invert Layer")))

  (gimp-image-undo-group-start img)

;; Real work goes in here

   (gimp-image-add-layer img invert-layer -1)
   (if (= invert-type 0)
    (gimp-invert inLayer)
       (gimp-invert invert-layer))
   (gimp-layer-set-mode invert-layer
            (cond
              ((= invert-type 0) HUE-MODE)
              ((= invert-type 1) HUE-MODE)
              ((= invert-type 2) NORMAL-MODE)
              ((= invert-type 3) DARKEN-ONLY-MODE)
              ((= invert-type 4) VALUE-MODE)))


   (gimp-drawable-set-name (car (gimp-image-merge-down img invert-layer EXPAND-AS-NECESSARY))
            (cond
              ((= invert-type 0) "GM Light Invert")
              ((= invert-type 1) "GM Color Only Invert")
              ((= invert-type 2) "GM Simple Color Invert")
                 ((= invert-type 3) "GM Solarize")
                 ((= invert-type 4) "GM Vivid V-Invert")))

  (gimp-image-undo-group-end img)
  (gimp-displays-flush)

  )
)

;; Register scripts

(script-fu-register
 "script-fu-gm-color-invert 0"
 _"<Image>/FX-Foundry/Color/Lasm's Light Invert"
 "Version 2.0\nLasm's color invert effect. This turns the photo into a photo negative without changing the colors and the effect is reversible.
  Cycle through all 3 effects in random order and you will arrive back to the original image"
 "lasm"
 "Copyright 2002-2005, lasm"
 "February 20, 2002"
 "RGB*"
 SF-IMAGE "The Image"      0
 SF-DRAWABLE "The Layer" 0
)

(script-fu-register
 "script-fu-gm-color-invert 1"
 _"<Image>/FX-Foundry/Color/Lasm's Color Only Invert"
 "Version 2.0\nLasm's color invert effect. This inverts color only, leaving brightness alone on any RGB image and the effect is reversible.
 Cycle through all 3 effects in random order and you will arrive back to the original image"
 "lasm"
 "Copyright 2002-2005, lasm"
 "February 20, 2002"
 "RGB*"
 SF-IMAGE "The Image"      0
 SF-DRAWABLE "The Layer" 0
)

(script-fu-register
 "script-fu-gm-color-invert 2"
 _"<Image>/FX-Foundry/Color/Lasm's Simple Color Invert"
 "Version 2.0\nLasm's color invert effect. This is vanilla color invert and the effect is reversible.
 Cycle through all 3 effects in random order and you will arrive back to the original image"
 "lasm"
 "Copyright 2002-2005, lasm"
 "February 20, 2002"
 "RGB*"
 SF-IMAGE "The Image"      0
 SF-DRAWABLE "The Layer" 0
)

(script-fu-register
 "script-fu-gm-color-invert 3"
 _"<Image>/FX-Foundry/Color/Lasm's Asymmetric Solarize"
 "Version 2.0\nThe solarize effect is irreversible."
 "lasm"
 "Copyright 2002-2005, lasm"
 "November 19, 2005"
 "RGB*"
 SF-IMAGE "The Image"      0
 SF-DRAWABLE "The Layer" 0
)

(script-fu-register
 "script-fu-gm-color-invert 4"
 _"<Image>/FX-Foundry/Color/Lasm's Vivid V-Invert"
 "Version 2.0\nProduces a highly saturated version of v-invert. Compare this image with the regular plug-in-vinvert ! Toggle it twice and it will stay in the V-invert mode, but the effect is not reversible to the original image."
 "lasm"
 "Copyright 2002-2005, lasm"
 "November 21, 2005"
 "RGB*"
 SF-IMAGE "The Image"      0
 SF-DRAWABLE "The Layer" 0
)

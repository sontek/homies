;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Wrap paint effect script  for GIMP 2.4
; Original author: Iccii <iccii@hotmail.com>
;
; Tags: effects
;
; Author statement:
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; version 0.1  by Iccii 2001/04/15 <iccii@hotmail.com>
;     - Initial relase
; version 0.2  by Iccii 2001/10/01 <iccii@hotmail.com>
;     - Changed menu path because this script attempts to PS's filter
;     - Added some code (if selection exists...)
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

(define (script-fu-wrap-effect    inImage
                inDrawable
                inRadius
                inGamma1
                inGamma2
                inSmooth
    )
  (let* (
          (theNewlayer 0)
          (theOldselection 0)
          (theLayermask 0)
        )
    (gimp-image-undo-group-start inImage)

    (gimp-context-push)
    (set! theNewlayer (car (gimp-layer-copy inDrawable 1)))
    (set! theOldselection (car (gimp-selection-save inImage)))
    (gimp-drawable-set-name theNewlayer "Wrap effect")
    (gimp-layer-set-mode theNewlayer NORMAL-MODE)
    (gimp-image-add-layer inImage theNewlayer -1)

    (gimp-desaturate theNewlayer)
    (plug-in-gauss-iir2 1 inImage theNewlayer inRadius inRadius)
    (plug-in-edge 1 inImage theNewlayer 10.0 1 0)
    (gimp-invert theNewlayer)

    ; ������theNewlayer�����V���M�[���������������

    (if (eqv? inSmooth TRUE)
        (plug-in-gauss-iir2 0 inImage theNewlayer 5 5))
    (gimp-edit-copy theNewlayer)

    (if (< 0 (car (gimp-layer-get-mask theNewlayer)))
        (gimp-layer-remove-mask theNewlayer MASK-APPLY))
    (set! theLayermask (car (gimp-layer-create-mask theNewlayer ADD-BLACK-MASK)))
    (gimp-layer-add-mask theNewlayer theLayermask)
    (gimp-floating-sel-anchor (car (gimp-edit-paste theLayermask 0)))

    (gimp-levels theNewlayer 0 0 255 (/ inGamma1 10) 0 255)
    (gimp-levels theNewlayer 0 0 255 (/ inGamma1 10) 0 255)
    (gimp-levels theLayermask 0 0 255 (/ inGamma2 10) 0 255)
    (gimp-levels theLayermask 0 0 255 (/ inGamma2 10) 0 255)

    (gimp-layer-remove-mask theNewlayer MASK-APPLY)
    (gimp-selection-load theOldselection)
    (gimp-edit-copy theNewlayer)
    (gimp-image-remove-layer inImage theNewlayer)
    (gimp-floating-sel-anchor (car (gimp-edit-paste inDrawable 0)))
    (gimp-selection-load theOldselection)
    (gimp-image-remove-channel inImage theOldselection)

    ;(gimp-image-set-active-layer inImage inDrawable)
    (gimp-image-undo-group-end inImage)
    (gimp-displays-flush)
    (gimp-context-pop)
    )
)

(script-fu-register
    "script-fu-wrap-effect"
    "<Image>/FX-Foundry/Artistic/Wrap Effect..."
    "Draws with wrap effect, which simulates Photoshop's Wrap filter"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "Oct, 2001"
    "RGB*"
    SF-IMAGE    "Image"            0
    SF-DRAWABLE    "Drawable"        0
    SF-ADJUSTMENT    "Randomness"        '(10 0 32 1 10 0 0)
    SF-ADJUSTMENT    "Highlight Balance"    '(3.0 1.0 10 0.5 0.1 1 0)
    SF-ADJUSTMENT    "Edge Amount"        '(3.0 1.0 10 0.5 0.1 1 0)
    SF-TOGGLE    "Smooth"        FALSE
)

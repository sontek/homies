;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Graduated filter script  for GIMP 2.4
; Copyright (C) 2004 Kevin Payne paynekj@hotmail.com
;
; Tags: filer, nd
;
; Author statement:
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Version 0.1 14.04.2004 First Go
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


; A debug routine to place some text into the image
(define (place_comment img adraw xpos ypos text)
      (set! text-float (car (gimp-text-fontname img adraw xpos ypos text 0 1 20 0 "-*-*courier*-*-r-*-*-*-*-*-*-*-*-*-*")))
      (gimp-floating-sel-anchor text-float)
)

; this is the main routine
(define (script-fu-grad_kp aimg adraw colour grad_rev)
  (let* (
         (rval (/ (* 1.0 (car colour)) 255.0))
         (gval (/ (* 1.0 (car      (cdr colour))) 255.0))
         (bval (/ (* 1.0 (car (cdr (cdr colour)))) 255.0))
         (height (car (gimp-drawable-height adraw)))
         (l-mask 0)
         (new-layer 0)
        )

   (gimp-image-undo-group-start aimg)
   (gimp-context-push)
;  (place_comment img adraw 10 40 "got here")

; Create a duplicate of the original layer and re-name it
   (set! new-layer (car (gimp-layer-copy adraw TRUE)))
   (gimp-image-add-layer aimg new-layer 0)
   (gimp-drawable-set-name new-layer "Filtered layer")

;   (place_comment img adraw 10 40 (number->string rval 10))
;   (place_comment img adraw 10 80 (number->string gval 10))
;   (place_comment img adraw 10 120 (number->string bval 10))

; filter the colours of the new layer
   (plug-in-colors-channel-mixer 1 aimg new-layer FALSE rval 0 0   0 gval 0   0 0 bval)

; create a layer mask
   (set! l-mask (car (gimp-layer-create-mask new-layer 0)))
   (gimp-layer-add-mask new-layer l-mask)

; set the foreground and background colours to their default values of black and white
   (gimp-context-set-default-colors)

; apply a gradient to the layer mask
   (gimp-context-set-gradient "FG to BG (RGB)")
   (gimp-edit-blend l-mask 0 0 0 100.0 0.0 0 grad_rev FALSE 0 0.0 TRUE 0 0 0 height)


; flush the image
   (gimp-displays-flush)

   (gimp-image-undo-group-end aimg)
   (gimp-context-pop)
  )
)

(script-fu-register "script-fu-grad_kp"
            _"<Image>/FX-Foundry/Photo/Enhancement/Graduated Filter..."
            "Add a graduated filter of an image"
            "Kevin Payne <paynekj@hotmail.com>"
            "Kevin Payne"
            "14/04/2004"
            "RGB*"
            SF-IMAGE "Input Image" 0
            SF-DRAWABLE "Input Drawable" 0
            SF-COLOR _"Filter Colour" '(255 204 153)
            SF-TOGGLE     _"Gradient Reverse"   TRUE
            )

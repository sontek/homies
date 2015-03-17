;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Under Water red correction script  for GIMP 2.4
; Original author: meddler (www.tankedup-imaging.com)
;
; Tags: photo, underwater, red
;
; Author statement:
;
; 06 July 2006
; A Script-Fu script to compensate for underwater colour loss
; at greater than 5m
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

(define (script-fu-med img photo)
   (let*
        (
         (uw-layer 0)
         (uw1-layer 0)
        )
    (gimp-undo-push-group-start img)

    ;first copy of photo, set foreground to grey, fill with grey and set mode to color
    (set! uw-layer (car (gimp-layer-copy photo 1)))
    (gimp-image-add-layer img uw-layer -1)
    (gimp-layer-set-name uw-layer "red enhance 2")
    (gimp-context-set-foreground '(128 128 128))
    (gimp-edit-fill uw-layer 0)
    (gimp-layer-set-mode uw-layer COLOR)

    ;second copy of photo, set background to red, fill with red and set mode to multiply
    (set! uw1-layer (car (gimp-layer-copy photo 1)))
    (gimp-image-add-layer img uw1-layer -1)
    (gimp-layer-set-name uw1-layer "red enhance 2a")
    (gimp-context-set-background '(255 0 0))
    (gimp-edit-fill uw1-layer 1)
    (gimp-layer-set-mode uw1-layer MULTIPLY)

    ;merge down red to grey (clip-to-image) change mode to screen
    (set! uw-layer (car (gimp-image-merge-down img uw1-layer 1)))
    (gimp-layer-set-mode uw-layer SCREEN)

    ;set opacity to 50%, tweak up for deep (very green images) or down for shallow
    (gimp-layer-set-opacity uw-layer 50)

    ;clean
    (gimp-undo-push-group-end img)
    (gimp-displays-flush)
    )
)


(script-fu-register "script-fu-med"
        _"<Image>/FX-Foundry/Photo/Enhancement/Underwater _Red Enhance"
        "Based on the mandrake method for Photoshop. Translated to GIMP by meddler. See www.scubaboard.com for the original articles."
        "www.tankedup-imaging.com"
        "Tanked Up Underwater Imaging"
        "06 July 2006"
        ""
        SF-IMAGE "Image" 0
        SF-DRAWABLE "Drawable" 0
)

;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Texturizer script  for GIMP 2.4
; Copyright (C) 2001 Iccii <iccii@hotmail.com>
;
; Tags: texturize
;
; Author statement:
;
; I would appreciate any comments/suggestions that you have about this
; script. I need new texture, how to create it.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; version 0.1  by Iccii 2001/09/26 <iccii@hotmail.com>
;     - Initial relase
;     - There are three texture type -- Sand, Paper, Cloud
; version 0.2  by Iccii 2001/09/27 <iccii@hotmail.com>
;     - Create the texture image as new window image instead of
;       creating layer in base image
;     - Added Depth option
; version 0.2a by Iccii 2001/09/30 <iccii@hotmail.com>
;     - Added Pattern option in texture type
; version 0.3  by Iccii 2001/10/01 <iccii@hotmail.com>
;     - Changed menu path because this script attempts to PS's filter
;     - Added Angle option
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


(define (script-fu-texturizer
            img
            drawable
            pattern
            bg-type
            angle
            elevation
            direction
            invert?
            show?
    )
  (let* (
     (width (car (gimp-drawable-width drawable)))
     (height (car (gimp-drawable-height drawable)))
     (old-fg (car (gimp-context-get-foreground)))
     (old-pattern (car (gimp-context-get-pattern)))
     (tmp-image (car (gimp-image-new width height GRAY)))
     (tmp-layer (car (gimp-layer-new tmp-image width height
                                         GRAY-IMAGE "Texture" 100 NORMAL-MODE)))
        ) ; end variable definition

    (gimp-image-undo-group-start img)
    (gimp-image-undo-disable tmp-image)
   ; (if (eqv? (car (gimp-drawable-is-layer-mask drawable)) TRUE)
   ;     (set! layer (car (gimp-image-get-active-layer img drawable))))
    (gimp-drawable-fill tmp-layer WHITE-FILL)
    (gimp-image-add-layer tmp-image tmp-layer 0)

    (cond
      ((eqv? bg-type 0)

         (gimp-context-set-pattern pattern)
         (gimp-edit-bucket-fill tmp-layer PATTERN-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0))
      ((eqv? bg-type 1)
         (plug-in-noisify 1 img tmp-layer FALSE 1.0 1.0 1.0 0)
         (gimp-brightness-contrast tmp-layer 0 63))
      ((eqv? bg-type 2)
         (plug-in-solid-noise 1 img tmp-layer FALSE FALSE (rand 65535) 15 16 16)
         (plug-in-edge 1 img tmp-layer 4 1 0)
         (gimp-brightness-contrast tmp-layer 0 -63))
      ((eqv? bg-type 3)
         (plug-in-plasma 1 img tmp-layer (rand 65535) 4.0)
         (plug-in-gauss-iir2 1 img tmp-layer 1 1)
         (gimp-brightness-contrast tmp-layer 0 63))
      ) ; end of cond
    (plug-in-bump-map 1 img drawable tmp-layer angle (+ 35 elevation)
                      1 0 0 0 0 TRUE invert? GRADIENT-LINEAR)



   ; (cond
    ;; If Drawable is Layer
   ;   ((eqv? (car (gimp-drawable-is-layer drawable)) TRUE)
   ;     (gimp-image-set-active-layer img drawable))
    ;; If Drawable is Layer mask
   ;   ((eqv? (car (gimp-drawable-is-layer-mask drawable)) TRUE)
   ;     (gimp-image-set-active-layer img layer))
    ;; If Drawable is Channel
   ;   ((eqv? (car (gimp-drawable-is-channel drawable)) TRUE)
   ;     (gimp-image-set-active-channel img drawable))
   ; ) ; end of cond

    (gimp-context-set-foreground old-fg)
    (gimp-context-set-pattern old-pattern)
    (gimp-image-clean-all tmp-image)
    (gimp-image-undo-enable tmp-image)
    (if (eqv? show? TRUE)
        (gimp-display-new tmp-image)
        (gimp-image-delete tmp-image))
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
  "script-fu-texturizer"
  "<Image>/FX-Foundry/Selection Effects/Texturizer..."
  "Creates textured canvas image, which simulates Photoshop's Texturizer filter"
  "Iccii <iccii@hotmail.com>"
  "Iccii"
  "2001, Oct"
  "RGB* GRAY*"
  SF-IMAGE      "Image"               0
  SF-DRAWABLE   "Drawable"         0
  SF-PATTERN    "Use Pattern"      "Pine?"
  SF-OPTION     "Texture Type"     '("Pattern" "Sand" "Paper" "Cloud")
  SF-ADJUSTMENT "Angle"            '(135 0 360 1 10 0 0)
  SF-ADJUSTMENT "Depth"            '(0 -5 5 1 1 0 1)
  SF-OPTION     "Stretch Direction" '("None" "Horizontal" "Vertical")
  SF-TOGGLE     "Invert"           FALSE
  SF-TOGGLE     "Show Texture"     FALSE
)

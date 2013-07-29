;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Auto Contrast Correction script  for GIMP 2.4
; Original author: Mark Lowry
;
; Tags: color, photo, exposure
;
; Author statement:
;
; Automatically adjusts contrast of the current
; drawable by duplicating the layer, setting the
; new layer to Value blend mode, then running
; Auto Levels on the Value layer.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Created 4/5/2006
; Revised 10/26/2007 to fix unbound variables (required for v.2.4.0)
; Revised 10/31/2007 - added option to skip autolevels for later manual adjustment - Alexia Death
; Revised 11/01/2007 - Added Hue correction layer - Alexia Death
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



(define (script-fu-auto-contrast img drawable merge-flag alskip-flag)

   (let* (
         (value-layer 0)
         (hue-layer 0)
         )


   ; Start an undo group.  Everything between the start and the end
   ; will be carried out if an undo command is issued.

   (gimp-image-undo-group-start img)

   ;; CREATE THE VALUE LAYER ;;

   ; Create a new layer

   (set! value-layer (car (gimp-layer-copy drawable 0)))
   (set! hue-layer (car (gimp-layer-copy drawable 0)))

   ; Give it a name

   (gimp-drawable-set-name value-layer "Contrast Adjustment Layer")
   (gimp-drawable-set-name hue-layer "Hue Correction Layer")

   ; Add the new layer to the image

   (gimp-image-add-layer img value-layer 0)
   (gimp-image-add-layer img hue-layer 0)

   ; Set opacity to 100%

   (gimp-layer-set-opacity value-layer 100)
   (gimp-layer-set-opacity hue-layer 100)

   ; Set the layer modes to Value and hue

   (gimp-layer-set-mode value-layer VALUE-MODE)
   (gimp-layer-set-mode hue-layer HUE-MODE)

   ; Adjust contrast
   (if (equal? alskip-flag FALSE)
     (gimp-levels-stretch value-layer)
   )
   ; Merge down, if required

   (if (equal? merge-flag TRUE)
      (begin
       (gimp-image-merge-down img value-layer 1)
       (gimp-image-merge-down img hue-layer 1)
       )
   )

   ;

   ; Complete the undo group

   (gimp-image-undo-group-end img)

   ; Flush the display

   (gimp-displays-flush)

   )

)


(script-fu-register "script-fu-auto-contrast"
      "<Image>/FX-Foundry/Photo/Enhancement/Auto contrast correction"
      "Automatically adjust contrast of drawable. Brightens dull photos."
      "Mark Lowry"
      "Mark Lowry"
      "2007"
      "RGB*, GRAY*"
      SF-IMAGE "Image" 0
      SF-DRAWABLE "Current Layer" 0
      SF-TOGGLE "Merge Layers"  FALSE
      SF-TOGGLE "Skip auto levels"  FALSE
 )


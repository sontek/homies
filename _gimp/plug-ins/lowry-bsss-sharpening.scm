;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; lyle's BSSS script  for GIMP 2.4
; Original author: Mark Lowry
;
; Tags: photo, sharpen
;
; Author statement:
;
; A GIMP script-fu to perform the sharpening
; technique created by lylejk of dpreview.com

; Creates a layer set to screen mode and
; another layer above it set to subtract mode.
; The subtract layer is then blurred
; and merged down with the screen layer, the
; result of which is merged down with the original
; layer (if so selected).
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Created 3/25/2006
;
; Revised 4/5/2006 to add option not to merge the screen layer
; down with the original drawable.  This allows the user to
; duplicate the screen layer, as desired, to provide further
; incremental sharpening.
;
; Revised 10/26/2007 to fix unbound variables (required by v.2.4.0)
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



(define (script-fu-BSSS   img drawable blur-rad merge-flag )

   (let* (
         (screen-layer 0)
         (subtract-layer 0)
         (first-merge 0)
         )

   ; Start an undo group.  Everything between the start and the end
   ; will be carried out if an undo command is issued.

   (gimp-image-undo-group-start img)

   ;; RENAME THE LAYER TO BE SHARPENED

   (gimp-drawable-set-name drawable "BSSS Sharpened")

   ;; CREATE THE SCREEN LAYER ;;

   (set! screen-layer (car (gimp-layer-copy drawable 0)))

   ; Give it a name

   (gimp-drawable-set-name screen-layer "Screen")

   ; Add the new layer to the image

   (gimp-image-add-layer img screen-layer 0)

   ; Set opacity to 100%

   (gimp-layer-set-opacity screen-layer 100)

   ;; CREATE THE SUBTRACT LAYER ;;

   (set! subtract-layer (car (gimp-layer-copy drawable 0)))

   ; Name the subtract layer

   (gimp-drawable-set-name subtract-layer "Subtract")

   ; Add the new layer to the image

   (gimp-image-add-layer img subtract-layer 0)

   ; Set opacity to 100%

   (gimp-layer-set-opacity subtract-layer 100)

   ; Set the layer mode to subtract

   (gimp-layer-set-mode subtract-layer 8)

   ; Blur the subtract layer

   (plug-in-gauss-iir 1 img subtract-layer blur-rad 1 1 )

   ;; NOW MERGE EVERYTHING BACK DOWN TO THE ORIGINAL LAYER ;;

   ; Merge down with screen layer and set the layer mode to Screen

   (set! first-merge (car(gimp-image-merge-down img subtract-layer 1)) )

   (gimp-layer-set-mode first-merge 4)

   ; Merge down with the drawable, if selection box was checked.

   (if (equal? merge-flag TRUE)

      (gimp-image-merge-down img first-merge 1 )

      ()

   )

   ; Complete the undo group

   (gimp-image-undo-group-end img)

   ; Flush the display

   (gimp-displays-flush)

   )

)


(script-fu-register "script-fu-BSSS"
      "<Image>/FX-Foundry/Photo/Sharpen/BSSS Sharpening"
      "Creates a layer set to screen mode and another layer above it set to subtract mode. The subtract layer is then blurred and merged down with the screen layer, the result of which is merged down with the original layer (if so selected).  You may duplicate the screen layer as many times as desired to increase the effect."
      "Mark Lowry"
      "technique by lylejk of dpreview.com"
      "2007"
      "RGB*, GRAY*"
      SF-IMAGE "Image" 0
      SF-DRAWABLE "Current Layer" 0
      SF-VALUE "Gaussian Blur Radius"  "5"
      SF-TOGGLE "Merge Layers?"  TRUE
 )


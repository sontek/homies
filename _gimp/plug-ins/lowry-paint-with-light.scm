;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Paint with Light scripts  for GIMP 2.4
; Original author: Mark Lowry
;
; Tags: photo, tool
;
; Author statement:
;
; Two layer version;
; Creates a black layer set to dodge mode and
; a white layer set to burn mode.
; Painting on the black layer will add lighter
; regions to the layer below, and painting on
; the white layer will add darker regions to
; the layer below.
;
; One layer version:
; Creates a 50% gray layer set to overlay mode.
; Painting on the layer in white will lighten
; regions below, and painting on the layer in black
; will will darken the regions below.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Created 3/20/2006
; Revised 3/21/2006 to add undo group.
; Revised 4/8/2006 to make better use of GIMP's existing functions
; Revised 10/26/2007 to define unbound variables (required with v.2.4.0)
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

(define (script-fu-Paintwithlight  img drawable )

   (let* (
         (new-layer 0)
         )


   ; Start an undo group.  Everything between the start and the end
   ; will be carried out if an undo command is issued.

   (gimp-image-undo-group-start img)

   ;; CREATE THE BLACK LAYER SET TO DODGE MODE ;;

   ; Create a new layer

   (set! new-layer (car (gimp-layer-new   img
                                          (car(gimp-image-width  img))
                                          (car(gimp-image-height img))
                                          RGB
                                          "Paint Light"
                                          100
                                          DODGE-MODE
                         )
                    )
   )

   ; Add the new layer to the image

   (gimp-image-add-layer img new-layer 0)

   ; Fill new layer with White

   (gimp-drawable-fill new-layer WHITE-FILL )

   ; Invert the new layer to make it Black

   (gimp-invert new-layer)

   ;

   ;

   ;; CREATE THE WHITE LAYER SET TO BURN MODE ;;

   ; Create another layer

   (set! new-layer (car (gimp-layer-new   img
                                          (car(gimp-image-width  img))
                                          (car(gimp-image-height img))
                                          RGB
                                          "Paint Dark"
                                          100
                                          BURN-MODE
                         )
                    )
   )

   ; Add the new layer to the image

   (gimp-image-add-layer img new-layer 0)

   ; Fill new layer with White

   (gimp-drawable-fill new-layer WHITE-FILL )

   ; Complete the undo group

   (gimp-image-undo-group-end img)

   ; Flush the display

   (gimp-displays-flush)

   )

)


(script-fu-register "script-fu-Paintwithlight"


      "<Image>/FX-Foundry/Light and Shadow/Paint with Light(two layers)"

      "Add black dodge layer and white burn layer.  Painting on the black layer will add lighter regions to the layer below, and painting on the white layer will add darker regions to the layer below.  BEST WHEN USED GENTLY!  Try soft brush set to 3% opacity."

      "Mark Lowry"

      "Mark Lowry"

      "2007"

      "RGB*, GRAY*"

      SF-IMAGE "Image" 0

      SF-DRAWABLE "Current Layer" 0

 )

(define (script-fu-Paintwithlight2  img drawable )

   (let* (
         (new-layer 0)
         (oldforeground 0)
         )

   ; Start an undo group.  Everything between the start and the end
   ; will be carried out if an undo command is issued.

   (gimp-image-undo-group-start img)

   ;; CREATE THE 50% GRAY LAYER SET TO OVERLAY MODE ;;

   ; Create a new layer

   (set! new-layer (car (gimp-layer-new   img
                                          (car(gimp-image-width  img))
                                          (car(gimp-image-height img))
                                          RGB
                                          "PWL2"
                                          100
                                          OVERLAY-MODE
                         )
                    )
   )

   ; Add the new layer to the image

   (gimp-image-add-layer img new-layer 0)


   ; Make note of the current foreground color

   (set! oldforeground (gimp-context-get-foreground) )


   ; Fill new layer with 50% gray

   ; First, set the foreground color to 50% gray

   (gimp-context-set-foreground '(128 128 128) )

   ; Now, fill the layer with the foreground color

   (gimp-drawable-fill new-layer FOREGROUND-FILL )

   ; Reset the foreground color to the old foreground color

   (gimp-context-set-foreground (car oldforeground) )

   ; Complete the undo group

   (gimp-image-undo-group-end img)

   ; Flush the display

   (gimp-displays-flush)

   )

)


(script-fu-register "script-fu-Paintwithlight2"

      "<Image>/FX-Foundry/Light and Shadow/Paint with Light(one layer)"

      "Add 50% gray overlay layer.  Painting on the layer in white will lighten regions below, and painting on the layer in black will will darken the regions below.  BEST USED GENTLY!  Try soft brush set to 3% opacity."

      "Mark Lowry"

      "Mark Lowry"

      "2007"

      "RGB*, GRAY*"

      SF-IMAGE "Image" 0

      SF-DRAWABLE "Current Layer" 0

 )



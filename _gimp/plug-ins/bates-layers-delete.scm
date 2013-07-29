;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Mass delete layers script  for GIMP 2.4
; Created by Daniel Bates
;
; Tags: public domain, layers, delete
;
; Author statement:
;
; Script designed to mass delete layers from current image
; User uses numbers to denote start and end point of deletion
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;
; --------------------------------------------------------------------
;
; This script is released into the public domain.
; You may redistribute and/or modify this script or extract segments without prior consent.

; This script is distributed in the hope of being useful
; but without warranty, explicit or otherwise.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Define Script

(define (script-fu-delete-layers theImage theDraw theLayer1 theLayer2)

; Define Variables

(let*
(
    (theNumber 0)
    (theRepeat 0)
    (theLayerRef 0)
 )

; If the end layer is set below the start layer create an error message and terminate
(if (> theLayer1 theLayer2)
(begin
(set! theLayerRef (car (gimp-message-get-handler)))
(gimp-message-set-handler 0)
(gimp-message "Error: End layer number must be set higher than start layer number!")
(gimp-message-set-handler theLayerRef))
(begin

; Begin an undo group
(gimp-undo-push-group-start theImage)

; Get the number of layers in an image and set to a variable
(set! theNumber (car (gimp-image-get-layers theImage)))

; If layer2 is set above total layers change layer2 value to the total number of layers
(if (> theLayer2 theNumber)
(set! theLayer2 theNumber))

; Set the repeat variable by subtracting the user input values from the total number of layers
(set! theRepeat (+ (- theLayer2 theLayer1) 1))

; Begin loop and continue while repeat is higher than zero
(while (> theRepeat 0)

    ; Set up variable for setting active layers and attributes
    (set! theLayerRef (cadr (gimp-image-get-layers theImage)))

    ; Alter theNumber for use in setting active layers and attributes
    (set! theNumber (car (gimp-image-get-layers theImage)))
    (set! theNumber (- theNumber (- theLayer1 1)))

    ; Set the layer to be editted as the active layer
    ; (set! theDraw (gimp-image-set-active-layer theImage (aref theLayerRef (- theNumber 1))))

    ; Delete the specified layer
    (gimp-image-remove-layer theImage (aref theLayerRef (- theNumber 1)))

    ; Alter repeat variable ready for checking for next layer, if applicable
    (set! theRepeat (- theRepeat 1))

)

; Update visual display
(gimp-displays-flush)

; End undo group
(gimp-undo-push-group-end theImage)

))
))

; Register script
(script-fu-register     "script-fu-delete-layers"
            "<Image>/FX-Foundry/Multi-Layer Tools/Delete Layers..."
            "Deletes layers within the specified range"
            "Daniel Bates"
            "Daniel Bates"
            "Dec 2007"
            "*"
            SF-IMAGE "SF-IMAGE" 0
            SF-DRAWABLE "SF-DRAWABLE" 0
            SF-ADJUSTMENT _"Start at which layer?" '(1 1 2000 1 5 0 1)
            SF-ADJUSTMENT _"End at which layer?" '(2 1 2000 1 5 0 1)
)

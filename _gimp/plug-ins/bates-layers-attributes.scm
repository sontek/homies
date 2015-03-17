;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Mass edit layers attributes script  for GIMP 2.4
; Created by Daniel Bates
;
; Tags: public domain, layers, edit, attributes
;
; Author statement:
;
; Script designed to edit layer attributes for the specified layer range
; Layer name, mode, opacity, visible, linked and alpha lock can all be optionally set
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

(define (script-fu-modify-layers-attributes theImage theDraw theLayer1 theLayer2 ENcheck theLayerName ELMcheck theLayerMode EOcheck theLayerOpacity EVcheck theLayerVisible ELcheck theLayerLink EMTcheck theLayerMT)

; Define Variables

(let*
(
    (theNumber 0)
    (theRepeat 0)
    (theName 0)
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

; Adjust the value received for layer mode for direct input
(if (> theLayerMode 1)
(set! theLayerMode (+ theLayerMode 1)))

; Adjust the value received for visible for direct input
(if (= theLayerVisible 0)
(set! theLayerVisible 1)
(set! theLayerVisible 0))

; Set up variable for setting active layers and attributes
(set! theLayerRef (cadr (gimp-image-get-layers theImage)))

; Alter theNumber for use in setting active layers and attributes
(set! theNumber (- theNumber (- theLayer1 1)))

; Begin loop and continue while repeat is higher than zero
(while (> theRepeat 0)

    ; Edit layer name if requested by the user
    (if (= ENcheck TRUE)
    (gimp-drawable-set-name (aref theLayerRef (- theNumber 1)) (string-append theLayerName "#1")))

    ; Edit layer mode if requested by the user
    (if (= ELMcheck TRUE)
    (gimp-layer-set-mode (aref theLayerRef (- theNumber 1)) theLayerMode))

    ; Edit opacity if requested by the user
    (if (= EOcheck TRUE)
    (gimp-layer-set-opacity (aref theLayerRef (- theNumber 1)) theLayerOpacity))

    ; Edit layer visible property if requested by the user
    (if (= EVcheck TRUE)
    (gimp-drawable-set-visible (aref theLayerRef (- theNumber 1)) theLayerVisible))

    ; Edit linked property if requested by the user
    (if (= ELcheck TRUE)
    (gimp-drawable-set-linked (aref theLayerRef (- theNumber 1)) theLayerLink))

    ; Edit maintain transparency property if requested by the user
    (if (= EMTcheck TRUE)
    (gimp-layer-set-lock-alpha (aref theLayerRef (- theNumber 1)) theLayerMT))

    ; Alter variables ready for checking for next layer and applying to next layer
    ; (set! theLayer (+ theLayer 1))
    (set! theNumber (- theNumber 1))
    (set! theRepeat (- theRepeat 1))

)

; Update visual display
(gimp-displays-flush)

; End undo group
(gimp-undo-push-group-end theImage)

))
))

; Register script
(script-fu-register     "script-fu-modify-layers-attributes"
            "<Image>/FX-Foundry/Multi-Layer Tools/Edit Attributes of Layers..."
            "Edits layer attributes of the specified layer range"
            "Daniel Bates"
            "Daniel Bates"
            "Dec 2007"
            "*"
            SF-IMAGE "SF-IMAGE" 0
            SF-DRAWABLE "SF-DRAWABLE" 0
            SF-ADJUSTMENT _"Start at which layer?" '(1 1 2000 1 5 0 1)
            SF-ADJUSTMENT _"End at which layer?" '(2 1 2000 1 5 0 1)
            SF-TOGGLE _"Edit Layer Name?" FALSE
            SF-STRING "New Layer Name" "Layer"
            SF-TOGGLE _"Edit layer modes?" TRUE
            SF-OPTION _"New Layer Mode" '(_"Normal" _"Dissolve" _"Multiply" _"Screen" _"Overlay" _"Difference" _"Addition" _"Subtract" _"Darken Only" _"Lighten Only" _"Hue" _"Saturation" _"Color" _"Value" _"Divide" _"Dodge" _"Burn" _"Hard Light" _"Soft Light" _"Grain Extract" _"Grain Merge")
            SF-TOGGLE _"Edit opacity?" TRUE
            SF-ADJUSTMENT _"New Opacity" '(100 0 100 1 10 0 1)
            SF-TOGGLE _"Edit visible property?" TRUE
            SF-OPTION _"New Visible Property" '(_"Show Layers" _"Hide Layers")
            SF-TOGGLE _"Edit linked property?" TRUE
            SF-OPTION _"New Linked Property" '(_"Unlink Layers" _"Link Layers")
            SF-TOGGLE _"Edit maintain transparency property?" TRUE
            SF-OPTION _"Maintain Transparency?" '(_"No" _"Yes")
)

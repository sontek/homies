;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Number layers script  for GIMP 2.4
; Created by Daniel Bates
;
; Tags: public domain, layer names
;
; Author statement:
;
; Script designed to add number notation to the layers of the current image
; User chooses at what number layer to begin numbering
; Used mainly to identify which layers to affect with the other layer options scripts

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

(define (script-fu-number-layers theImage theDraw theLayer theLName)

; Define Variables

(let*
(
    (theNumber 0)
    (theRepeat 0)
    (theName 0)
    (theLayerRef 0)
 )


; Begin an undo group
(gimp-undo-push-group-start theImage)

; Get the number of layers in an image and set to a variable
(set! theNumber (car (gimp-image-get-layers theImage)))

; Set the repeat variable by subtracting the user input value from the total number of layers
(set! theRepeat (- theNumber (- theLayer 1)))

; If repeat is less than 1 produce error message and exit, otherwise continue script
(if (< theRepeat 1)
(begin
(set! theLayerRef (car (gimp-message-get-handler)))
(gimp-message-set-handler 0)
(gimp-message "Error: Input layer number exceeds total number of layers!")
(gimp-message-set-handler theLayerRef)
(gimp-undo-push-group-end theImage)
)
(begin

; If user input is higher than the number of layers in the image readjust user input to the number of layers
(if (> theLayer theNumber)
(set! theLayer theNumber))

; Set up variable for setting active layers and attributes
 (set! theLayerRef (cadr (gimp-image-get-layers theImage)))

; Alter theNumber for use in setting active layers and attributes
(set! theNumber (- theNumber (- theLayer 1)))

; Begin loop and continue while repeat is higher than zero
(while (> theRepeat 0)

    ; Set the layer to be editted as the active layer
    ; (set! theDraw (gimp-image-set-active-layer theImage (aref theLayerRef (- theNumber 1))))

    ; Get the layer's name and append a number to it
    (if (= theLName TRUE)
    (set! theName (string-append "(" (number->string theLayer) ")" "_" (car (gimp-drawable-get-name (aref theLayerRef (- theNumber 1))))))
    (set! theName (string-append "(" (number->string theLayer) ")" "_Layer")))

    ; Assign name to the layer
    (gimp-drawable-set-name (aref theLayerRef (- theNumber 1)) theName)

    ; Alter variables ready for checking for next layer and applying to next layer
    (set! theLayer (+ theLayer 1))
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
(script-fu-register     "script-fu-number-layers"
            "<Image>/FX-Foundry/Multi-Layer Tools/Number Layers..."
            "Edits layer names to number them. Numbering starts at user's specified number. Effect is culmulative if layers maintain original names."
            "Daniel Bates"
            "Daniel Bates"
            "Dec 2007"
            "*"
            SF-IMAGE "SF-IMAGE" 0
            SF-DRAWABLE "SF-DRAWABLE" 0
            SF-ADJUSTMENT _"Start at which layer?" '(1 1 2000 1 5 0 1)
            SF-TOGGLE _"Maintain current layer names?" TRUE
)

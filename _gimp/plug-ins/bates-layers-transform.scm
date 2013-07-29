;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Transform layers script  for GIMP 2.4
; Created by Daniel Bates
;
; Tags: public domain, layers, flip, offset, move
;
; Author statement:
;
; Script designed to apply transform actions for the specified layer range
; Layers can be rotated flipped, offset and moved, in that order
; Special spiral rotate available instead of a constant angle
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
;
; This script is distributed in the hope of being useful
; but without warranty, explicit or otherwise.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define Script

(define (script-fu-layers-transform theImage theDraw theLayer1 theLayer2 ARcheck SRcheck theLayerAngle AFcheck theLayerFlip AOcheck varX varY theEdge AMcheck varX2 varY2)

; Define Variables

(let*
(
    (theNumber 0)
    (theNumber2 0)
    (theRepeat 0)
    (theName 0)
    (theLayerRef 0)
    (theSR 0)
    (theRange 0)
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

; Set the range used for spiral rotate
(set! theRange (+ (- theLayer2 theLayer1) 1))

; Adjust the value received for edge behaviour for direct input
(if (= theEdge 0)
(set! theEdge 1)
(set! theEdge 0))

; Set up variable for setting active layers and attributes
(set! theLayerRef (cadr (gimp-image-get-layers theImage)))

; Set theNumber2 for use in setting active layers and attributes
(set! theNumber2 (- theNumber (- theLayer1 1)))

; Begin loop and continue while repeat is higher than zero
(while (> theRepeat 0)

    ; Rotate layer if requested by the user
    (if (= ARcheck TRUE)
    (begin
        (if (= SRcheck 1)
        (begin
        (gimp-drawable-transform-rotate (aref theLayerRef (- theNumber2 1)) (* (* (/ 360 theRange) theSR) (/ 3.14 180)) TRUE (/ (car (gimp-drawable-width (aref theLayerRef (- theNumber 1)))) 2) (/ (car (gimp-drawable-height (aref theLayerRef (- theNumber 1)))) 2) 1 2 FALSE 1 0)
        (set! theSR (+ theSR 1))
        )
        (begin
        (gimp-drawable-transform-rotate (aref theLayerRef (- theNumber2 1)) (* theLayerAngle (/ 3.14 180)) TRUE (/ (car (gimp-drawable-width (aref theLayerRef (- theNumber 1)))) 2) (/ (car (gimp-drawable-height (aref theLayerRef (- theNumber 1)))) 2) 1 2 FALSE 1 0)
        ))
))

    ; Flip layer if requested by the user
    (if (= AFcheck TRUE)
    (begin
        (if (= theLayerFlip 0)
        (gimp-drawable-transform-flip-simple (aref theLayerRef (- theNumber2 1)) 1 TRUE 0 FALSE))
        (if (= theLayerFlip 1)
        (gimp-drawable-transform-flip-simple (aref theLayerRef (- theNumber2 1)) 0 TRUE 0 FALSE))
        (if (= theLayerFlip 2)
        (gimp-drawable-transform-flip-simple (aref theLayerRef (- theNumber2 1)) 1 TRUE 0 FALSE)
        (gimp-drawable-transform-flip-simple (aref theLayerRef (- theNumber2 1)) 0 TRUE 0 FALSE))
    ))

    ; Offset layer if request by the user
    (if (= AOcheck TRUE)
    (gimp-drawable-offset (aref theLayerRef (- theNumber2 1)) theEdge 1 varX varY))

    ; Move layers if requested by the user
    (if (= AMcheck TRUE)
    (gimp-layer-set-offsets (aref theLayerRef (- theNumber2 1)) varX2 varY2))

    ; Alter variables ready for checking for next layer and applying to next layer
    (set! theNumber2 (- theNumber2 1))
    (set! theRepeat (- theRepeat 1))

)

; Update visual display
(gimp-displays-flush)

; End undo group
(gimp-undo-push-group-end theImage)

))
))

; Register script
(script-fu-register     "script-fu-layers-transform"
            "<Image>/FX-Foundry/Multi-Layer Tools/Transform Layers..."
            "Transforms layers from the specified layer range"
            "Daniel Bates"
            "Daniel Bates"
            "Dec 2007"
            "*"
            SF-IMAGE "SF-IMAGE" 0
            SF-DRAWABLE "SF-DRAWABLE" 0
            SF-ADJUSTMENT _"Start at which layer?" '(1 1 2000 1 5 0 1)
            SF-ADJUSTMENT _"End at which layer?" '(2 1 2000 1 5 0 1)
            SF-TOGGLE _"Rotate Layers?" TRUE
            SF-OPTION _"Spiral Rotate? (Rotate Angle will be ignored if yes)" '(_"No" _"Yes")
            SF-ADJUSTMENT _"Rotate Angle" '(0 -360 360 1 10 0 1)
            SF-TOGGLE _"Flip Layers?" TRUE
            SF-OPTION _"Flip Direction" '(_"Horizontally" _"Vertically" _"Both")
            SF-TOGGLE _"Offset Layer Contents?" TRUE
            SF-ADJUSTMENT _"Offset X" '(0 -2000 2000 1 10 0 1)
            SF-ADJUSTMENT _"Offset Y" '(0 -2000 2000 1 10 0 1)
            SF-OPTION _"Offset Edge Behaviour" '(_"Wrap" _"Transparent")
            SF-TOGGLE _"Move Layers?" TRUE
            SF-ADJUSTMENT _"Layer X Position" '(0 -2000 2000 1 10 0 1)
            SF-ADJUSTMENT _"Layer Y Position" '(0 -2000 2000 1 10 0 1)
)

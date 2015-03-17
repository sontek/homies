;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Scale and crop layers script  for GIMP 2.4
; Created by Daniel Bates
;
; Tags: public domain, layers, scale, crop
;
; Author statement:
;
; Script designed to apply scale or crop to the selected range of layers
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

(define (script-fu-layers-scalecrop theImage theDraw theLayer1 theLayer2 TypeCheck theBound theNewWidth theNewHeight varX varY theCenter)

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

; Set up variable for setting active layers and attributes
(set! theLayerRef (cadr (gimp-image-get-layers theImage)))

; Alter theNumber for use in setting active layers and attributes
(set! theNumber (- theNumber (- theLayer1 1)))

; Set up offset values for crop for direct usage if crop was selected
(if (= TypeCheck 1)
(begin
(set! varX (- (* varX 2)))
(set! varY (- (* varY 2)))
))

; Begin loop and continue while repeat is higher than zero
(while (> theRepeat 0)

; Check for type. Do first set of actions if Scale is requested. Otherwise do second set of actions.
(if (= TypeCheck 0)
(begin
    ; scale layer to requested dimensions
    (if (= theBound 0)
    (gimp-layer-scale (aref theLayerRef (- theNumber 1)) theNewWidth theNewHeight FALSE)
    (gimp-layer-scale (aref theLayerRef (- theNumber 1)) (gimp-image-width theImage) (gimp-image-height theImage) FALSE))
)
(begin
    ; Crop layer to requested dimensions
    (if (= theBound 0)
    (gimp-layer-resize (aref theLayerRef (- theNumber 1)) theNewWidth theNewHeight varX varY)
    (gimp-layer-resize-to-image-size (aref theLayerRef (- theNumber 1))))
)
)

    ; Place affected layers in the center if requested
    (if (= theCenter 0)
    (gimp-layer-set-offsets (aref theLayerRef (- theNumber 1)) (- (/ (car (gimp-image-width theImage)) 2) (/ (car (gimp-drawable-width (aref theLayerRef (- theNumber 1)))) 2)) (- (/ (car (gimp-image-height theImage)) 2) (/ (car (gimp-drawable-height (aref theLayerRef (- theNumber 1)))) 2))))

    ; Alter variables ready for checking for next layer and applying to next layer
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
(script-fu-register     "script-fu-layers-scalecrop"
            "<Image>/FX-Foundry/Multi-Layer Tools/Scale or Crop Layers..."
            "Scales or Crops layers from the specified layer range"
            "Daniel Bates"
            "Daniel Bates"
            "Dec 2007"
            "*"
            SF-IMAGE "SF-IMAGE" 0
            SF-DRAWABLE "SF-DRAWABLE" 0
            SF-ADJUSTMENT _"Start at which layer?" '(1 1 2000 1 5 0 1)
            SF-ADJUSTMENT _"End at which layer?" '(2 1 2000 1 5 0 1)
            SF-OPTION _"Scale or Crop?" '(_"Scale" _"Crop")
            SF-OPTION _"To Image Boundaries? (Further options are ignored if yes)" '(_"No" _"Yes")
            SF-ADJUSTMENT _"New Width" '(200 1 9000 1 5 0 1)
            SF-ADJUSTMENT _"New Height" '(200 1 9000 1 5 0 1)
            SF-ADJUSTMENT _"Offset X (for crop)" '(0 0 9000 1 5 0 1)
            SF-ADJUSTMENT _"Offset Y (for crop)" '(0 0 9000 1 5 0 1)
            SF-OPTION _"Center Adjusted Layers?" '(_"Yes" _"No")
)

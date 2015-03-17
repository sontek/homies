;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Layers copy to new script  for GIMP 2.4
; Created by Daniel Bates
;
; Tags: public domain, layers, copy
;
; Author statement:
;
; Script designed to mass copy layers from current image, add them to a new image and opens the new image
; User uses numbers to denote start and end point of copying
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

(define (script-fu-layers-copy-to-new theImage theDraw theLayer1 theLayer2)

; Define Variables

(let*
(
    (theNumber 0)
    (theRepeat 0)
    (theNewImage 0)
    (theType 0)
    (theHeight 0)
    (theWidth 0)
    (theLayerRef 0)

    (theNewLayer 0)
    (theLayerFloat 0)
    (theLayerCopy 0)
    (theLayerType 0)
    (theLayerMode 0)
    (theLayerName 0)
    (theLayerOpacity 0)
    (theLayerHeight 0)
    (theLayerWidth 0)
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

; Get the current image's type, height and width

(set! theType (car (gimp-image-base-type theImage)))
(set! theHeight (car (gimp-image-height theImage)))
(set! theWidth (car (gimp-image-width theImage)))

; Create the new image ready to receive layers

(set! theNewImage (car (gimp-image-new theWidth theHeight theType)))

; Begin loop and continue while repeat is higher than zero
(while (> theRepeat 0)

    ; Copy the current layer and grab its attributes
    (set! theLayerCopy (car (gimp-edit-copy (aref theLayerRef (- theNumber 1)))))
    (set! theLayerName (car (gimp-drawable-get-name (aref theLayerRef (- theNumber 1)))))
    (set! theLayerMode (car (gimp-layer-get-mode (aref theLayerRef (- theNumber 1)))))
    (set! theLayerOpacity (car (gimp-layer-get-opacity (aref theLayerRef (- theNumber 1)))))
    (set! theLayerType (car (gimp-drawable-type (aref theLayerRef (- theNumber 1)))))
    (set! theLayerHeight (car (gimp-drawable-height (aref theLayerRef (- theNumber 1)))))
    (set! theLayerWidth (car (gimp-drawable-width (aref theLayerRef (- theNumber 1)))))

    ; Create a new layer in the new image ready to receive layer contents
    (set! theNewLayer (car (gimp-layer-new theNewImage theLayerWidth theLayerHeight theLayerType theLayerName theLayerOpacity theLayerMode)))

    ; Add the new layer and move to the top of the stack
    (gimp-image-add-layer theNewImage theNewLayer -1)
    (gimp-drawable-fill theNewLayer 3)

    ; Paste contents into the new layer
    (set! theLayerFloat (car (gimp-edit-paste theNewLayer 0)))
    (gimp-floating-sel-anchor theLayerFloat)

    ; Alter variables ready for checking for next layer and applying to next layer
    (set! theNumber (- theNumber 1))
    (set! theRepeat (- theRepeat 1))

)

; Update visual display
(gimp-display-new theNewImage)
(gimp-displays-flush)

; End undo group
(gimp-undo-push-group-end theImage)

))
))

; Register script
(script-fu-register     "script-fu-layers-copy-to-new"
            "<Image>/FX-Foundry/Multi-Layer Tools/Copy Layers to New Image..."
            "Copies a range of layers and opens them as a new image"
            "Daniel Bates"
            "Daniel Bates"
            "Dec 2007"
            "*"
            SF-IMAGE "SF-IMAGE" 0
            SF-DRAWABLE "SF-DRAWABLE" 0
            SF-ADJUSTMENT _"Start at which layer?" '(1 1 2000 1 5 0 1)
            SF-ADJUSTMENT _"End at which layer?" '(2 1 2000 1 5 0 1)
)

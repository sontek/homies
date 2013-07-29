;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Gradient Difference script  for GIMP 2.4
; Created by Daniel Bates
;
; Tags: public domain, render, gradient
;
; Author statement:
;
; Adds a new layer and uses the gradient tool to generate a random design
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



;Define Script

(define (script-fu-grad-diff theImage theDraw theRepeat theStyle)

;Declare Variables

    (let*
    (
    (theHeight 0)
    (theWidth 0)
    (grad-layer 0)
    (theType 0)
    (theAlt 1)

    (varX1 0)
    (varX2 0)
    (varY1 0)
    (varY2 0)
    )

(gimp-context-push)

; Begin Undo Group

(gimp-undo-push-group-start theImage)

; Set height, width and type based on current image values

(set! theHeight (car (gimp-image-height theImage)))
(set! theWidth (car (gimp-image-width theImage)))
(set! theType (car (gimp-drawable-type-with-alpha theDraw)))

; Set a new layer

(set! grad-layer (gimp-layer-new theImage theWidth theHeight theType "Grad Diff #1" 100 0) )

; Fill the layer with Black

(gimp-context-set-background '(0 0 0))
(gimp-drawable-fill (car grad-layer) 1)

; Add the set layer

(gimp-image-add-layer theImage (car grad-layer) -1)

; Move the layer to the top of the stack and make it active

(gimp-image-raise-layer-to-top theImage (car grad-layer))
(gimp-image-set-active-layer theImage (car grad-layer))

; Set the Full saturation spectrum CCW gradient as the active one
(gimp-context-set-gradient "Full saturation spectrum CCW")

(while (> theRepeat 0)

; Assign random values to x and y variables for gradient tool usage

; Assign x and y values based on x-axis

(if (= theStyle 0)
(begin
(set! varX1 (- (random 1) (/ theWidth 2)))
(set! varX2 (+ (random theWidth) (* theWidth 1.5)))

(set! varY1 (- (random (* theHeight 3)) theHeight))
(if (>= varY1 (/ theHeight 2))
(set! varY2 (+ (random theHeight) theHeight)))
(if (< varY1 (/ theHeight 2))
(set! varY2 (- (random theHeight) theHeight)))
))

; Assign x and y values based on y-axis

(if (= theStyle 1)
(begin
(set! varY1 (- (random 1) (/ theHeight 2)))
(set! varY2 (+ (random theHeight) (* theHeight 1.5)))

(set! varX1 (- (random (* theWidth 3)) theWidth))
(if (>= varX1 (/ theWidth 2))
(set! varX2 (+ (random theWidth) theWidth)))
(if (< varX1 (/ theWidth 2))
(set! varX2 (- (random theWidth) theWidth)))
))

; Alternate how the x and y values are assigned

(if (= theStyle 2)
(begin
    (if (= theAlt 1)
    (begin
    (set! varX1 (- (random 1) (/ theWidth 2)))
    (set! varX2 (+ (random theWidth) (* theWidth 1.5)))

    (set! varY1 (- (random (* theHeight 3)) theHeight))
    (if (>= varY1 (/ theHeight 2))
    (set! varY2 (+ (random theHeight) theHeight)))
    (if (< varY1 (/ theHeight 2))
    (set! varY2 (- (random theHeight) theHeight)))
    (set! theAlt 2)
    ))

    (if (= theAlt 2)
    (begin
    (set! varY1 (- (random 1) (/ theHeight 2)))
    (set! varY2 (+ (random theHeight) (* theHeight 1.5)))

    (set! varX1 (- (random (* theWidth 3)) theWidth))
    (if (>= varX1 (/ theWidth 2))
    (set! varX2 (+ (random theWidth) theWidth)))
    (if (< varX1 (/ theWidth 2))
    (set! varX2 (- (random theWidth) theWidth)))
    (set! theAlt 1)
    ))
))

; Randomize how the x and y values are assigned

(if (= theStyle 3)
(begin
(set! theAlt (random 2))
    (if (= theAlt 0)
    (begin
    (set! varX1 (- (random 1) (/ theWidth 2)))
    (set! varX2 (+ (random theWidth) (* theWidth 1.5)))

    (set! varY1 (- (random (* theHeight 3)) theHeight))
    (if (>= varY1 (/ theHeight 2))
    (set! varY2 (+ (random theHeight) theHeight)))
    (if (< varY1 (/ theHeight 2))
    (set! varY2 (- (random theHeight) theHeight)))
    ))

    (if (= theAlt 1)
    (begin
    (set! varY1 (- (random 1) (/ theHeight 2)))
    (set! varY2 (+ (random theHeight) (* theHeight 1.5)))

    (set! varX1 (- (random (* theWidth 3)) theWidth))
    (if (>= varX1 (/ theWidth 2))
    (set! varX2 (+ (random theWidth) theWidth)))
    (if (< varX1 (/ theWidth 2))
    (set! varX2 (- (random theWidth) theWidth)))
    ))
))

; Apply gradient

(gimp-edit-blend (car grad-layer) 3 6 2 100 0 2 FALSE FALSE 0 0 TRUE varX1 varY1 varX2 varY2)

; Minus one from repeat count

(set! theRepeat (- theRepeat 1))

)

; Update display

(gimp-displays-flush)

; End Undo Group

(gimp-undo-push-group-end theImage)

(gimp-context-pop)

)
)

; Register Script

(script-fu-register     "script-fu-grad-diff"
            "<Image>/FX-Foundry/Artistic/Gradient Difference..."
            "Uses the gradient tool to generate a random design"
            "Daniel Bates"
            "Daniel Bates"
            "Dec 2007"
            "*"
            SF-IMAGE      "SF-IMAGE" 0
            SF-DRAWABLE   "SF-DRAWABLE" 0
            SF-ADJUSTMENT _"Repeat" '(5 1 200 1 3 0 1)
            SF-OPTION _"Style" '(_"X axis" _"Y axis" _"Alternating" _"Random")
)


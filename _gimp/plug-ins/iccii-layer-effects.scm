
;-------------------------------------------------------------------------
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Layer Effects script for GIMP 2.4
; Copyright (C) 2001 Iccii
;
; Tags: layer, effects
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
; version 0.1 by Iccii 2001/08/06
; - Initial relase
; version 0.2 by Iccii 2001/08/07
; - Divide each function into depending function and common function
; version 0.3 by Iccii 2001/08/09
; - Add the Stain script
; version 0.4 by Iccii 2001/08/10
; - Add the Color/Gradient/Pattern Overlay scripts
; version 0.5 by Iccii 2001/08/14
; - Add the Border script (not better)
; version 0.5a by Iccii 2003/05/29
; - Add "Antialias" option in Add Border script
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
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
; corrections made by Michael Hoelzen begin
; 2005-11-12 replaced some deprecated procedures (not all of them)
; 2005-11-12 error corrected in gradient overlay script:
; "do adaptive supersampling" and "supersampling threshold"
; weren't set
; corrections made by Michael Hoelzen end
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layer Effect common function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (layer-effects-common1
            img ;
            layer ;
            color ;
            angle ;
            offset-radius ;
            effect-radius ;
            blur-radius ;
            opacity ;
            layer-mode ;
            layer-name ;
            color-side ; (Outer(0) Inner(1) Inner-Inverse(2))
         )

    (let* (
        (width (car (gimp-drawable-width layer)))
        (height (car (gimp-drawable-height layer)))
        (radians (/ (* 2 *pi* angle) 360))
        (x-offset (* offset-radius (sin radians)))
        (y-offset (* offset-radius (cos radians)))
        (target-layer (car (gimp-layer-copy layer TRUE)))
        (effect-layer (car (gimp-layer-new img width height RGBA-IMAGE
        layer-name opacity layer-mode)))
        (effect-mask (car (gimp-layer-create-mask effect-layer WHITE-MASK)))
        (effect-mask2 0)
      )

        ;;
        (gimp-image-add-layer img effect-layer -1)
        ;replaced 2005-11-13
        ; (gimp-image-add-layer-mask img effect-layer effect-mask)
        (gimp-layer-add-mask effect-layer effect-mask)
        (if (= color-side 0)
        (gimp-invert effect-mask))
        (if (< 0 (car (gimp-layer-get-mask target-layer)))
        ;replaced 2005-11-13
        ; (gimp-image-remove-layer-mask img target-layer APPLY))
        (gimp-layer-remove-mask target-layer APPLY))

        ;;
        (gimp-context-set-background color)
        (gimp-drawable-fill effect-layer BG-IMAGE-FILL)
        (gimp-selection-layer-alpha target-layer)
        (if (= color-side 0)
            (gimp-selection-grow img effect-radius)
            (begin
                (gimp-selection-invert img)
                (gimp-selection-grow img effect-radius)
                (gimp-selection-invert img) ))
        (if (= color-side 0)
            (gimp-context-set-background '(255 255 255))
            (gimp-context-set-background '(0 0 0)))
            (gimp-edit-fill effect-mask BG-IMAGE-FILL)
            (gimp-selection-none img)
            ;;
            (gimp-context-set-background '(0 0 0))
        (if (< 0 offset-radius)
            (gimp-drawable-offset effect-mask FALSE OFFSET-BACKGROUND x-offset y-offset))
        (if (< 0 blur-radius)
            (plug-in-gauss-iir2 1 img effect-mask blur-radius blur-radius))
        (if (= color-side 2)
            (gimp-invert effect-mask))

        ;;
        ;replaced 2005-11-13
        ; (gimp-image-remove-layer-mask img effect-layer APPLY)
        (gimp-layer-remove-mask effect-layer APPLY)
        (set! effect-mask2 (car (gimp-layer-create-mask effect-layer BLACK-MASK)))
        ;replaced 2005-11-13
        ; (gimp-image-add-layer-mask img effect-layer effect-mask2)
        (gimp-layer-add-mask effect-layer effect-mask2)
        (if (= color-side 0)
            (gimp-invert effect-mask2))
        (gimp-selection-layer-alpha target-layer)
        (if (= color-side 0)
            (gimp-context-set-background '(0 0 0))
            (gimp-context-set-background '(255 255 255)))
        (gimp-edit-fill effect-mask2 BG-IMAGE-FILL)
        (gimp-selection-none img)

        ;;
        (if (= color-side 0)
            (gimp-image-lower-layer img effect-layer))
        (gimp-image-set-active-layer img layer)
        (list effect-layer)
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drop Shadow Script ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-layer-effects-drop-shadow
            img ;
            layer ;
            color ;
            angle ;
            offset-radius ;
            radius ;
            blur-radius ;
            opacity ;
            layer-mode ;
        )

   (let* (
        (old-bg (car (gimp-context-get-background)))
        (layer-mode (cond ((= layer-mode 0) 3)
        ((= layer-mode 2) 0)
        (layer-mode)))
      )
    (gimp-image-undo-group-start img)
    (layer-effects-common1 img layer color angle offset-radius radius blur-radius
    opacity layer-mode "Drop Shadow" 0)
    (gimp-context-set-background old-bg)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
   )
)

(script-fu-register
    "script-fu-layer-effects-drop-shadow"
    "<Image>/FX-Foundry/Layer Effects/Drop Shadow..."
    "Create the Drop Shadow on the layer with alpha"
    "Iccii "
    "Iccii"
    "Aug, 2001"
    "RGBA"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-COLOR "Shadow Color" '(0 0 0)
    SF-ADJUSTMENT "Lighting (degrees)" '(30 0 360 1 15 0 0)
    SF-ADJUSTMENT "Offset Radius" '(10 0 100 1 10 0 1)
    SF-ADJUSTMENT "Shadow Radius" '(0 0 100 1 10 0 1)
    SF-ADJUSTMENT "Shadow Blur Radius" '(5 0 100 1 10 0 1)
    SF-ADJUSTMENT "Drop Shadow Opacity" '(75 0 100 1 10 0 0)
    SF-OPTION "Shadow Layer Mode" '("Default (Multiply)" "Dissolve" "Normal"
    "Multiply" "Screen" "Overlay" "Difference"
    "Addition" "Subtract" "Darken" "Lighten"
    "Hue" "Saturation" "Color" "Value" "Divide")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inner Shadow script ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-layer-effects-inner-shadow
            img ;
            layer ;
            color ;
            angle ;
            offset-radius ;
            radius ;
            blur-radius ;
            opacity ;
            layer-mode ;
        )

    (let* (
            (old-bg (car (gimp-context-get-background)))
            (layer-mode (cond ((= layer-mode 0) 3)
            ((= layer-mode 2) 0)
            (layer-mode)))
        )
        (gimp-image-undo-group-start img)
        (layer-effects-common1 img layer color angle offset-radius radius blur-radius
        opacity layer-mode "Inner Shadow" 1)
        (gimp-context-set-background old-bg)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
    )
)
(script-fu-register
    "script-fu-layer-effects-inner-shadow"
    "<Image>/FX-Foundry/Layer Effects/Inner Shadow..."
    "Create the Inner Shadow on the layer with alpha"
    "Iccii "
    "Iccii"
    "Aug, 2001"
    "RGBA"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-COLOR "Shadow Color" '(0 0 0)
    SF-ADJUSTMENT "Lighting (degrees)" '(30 0 360 1 15 0 0)
    SF-ADJUSTMENT "Offset Radius" '(5 0 100 1 10 0 1)
    SF-ADJUSTMENT "Shadow Radius" '(0 0 100 1 10 0 1)
    SF-ADJUSTMENT "Shadow Blur Radius" '(5 0 100 1 10 0 1)
    SF-ADJUSTMENT "Drop Shadow Opacity" '(75 0 100 1 10 0 0)
    SF-OPTION "Shadow Layer Mode" '("Default (Multiply)" "Dissolve" "Normal"
    "Multiply" "Screen" "Overlay" "Difference"
    "Addition" "Subtract" "Darken" "Lighten"
    "Hue" "Saturation" "Color" "Value" "Divide")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outer Glow script ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-layer-effects-outer-glow
            img ;
            layer ;
            color ;
            radius ;
            blur-radius ;
            opacity ;
            layer-mode ;
        )

  (let* (
        (old-bg (car (gimp-context-get-background)))
        (layer-mode (cond ((= layer-mode 0) 4)
        ((= layer-mode 2) 0)
        (layer-mode)))
      )

    (gimp-image-undo-group-start img)
    (layer-effects-common1 img layer color 0 0 radius blur-radius
    opacity layer-mode "Outer Glow" 0)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
    "script-fu-layer-effects-outer-glow"
    "<Image>/FX-Foundry/Layer Effects/Outer Glow..."
    "Create the Outer Glow on the layer with alpha"
    "Iccii "
    "Iccii"
    "Aug, 2001"
    "RGBA"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-COLOR "Glow Color" '(255 255 191)
    SF-ADJUSTMENT "Glow Radius" '(2 0 100 1 10 0 1)
    SF-ADJUSTMENT "Blur Radius" '(10 0 100 1 10 0 1)
    SF-ADJUSTMENT "Opacity" '(75 0 100 1 10 0 0)
    SF-OPTION "Glow Layer Mode" '("Default (Screen)" "Dissolve" "Normal"
    "Multiply" "Screen" "Overlay" "Difference"
    "Addition" "Subtract" "Darken" "Lighten"
    "Hue" "Saturation" "Color" "Value" "Divide")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inner Glow script ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-layer-effects-inner-glow
            img ;
            layer ;
            color ;
            radius ;
            blur-radius ;
            opacity ;
            layer-mode ;
            glow-type ; Edge(0) or Inside(1)
        )

  (let* (
        (old-bg (car (gimp-context-get-background)))
        (layer-mode (cond ((= layer-mode 0) 4)
        ((= layer-mode 2) 0)
        (layer-mode)))
        (glow-type (+ glow-type 1))
      )

    (gimp-image-undo-group-start img)
    (layer-effects-common1 img layer color 0 0 radius blur-radius
    opacity layer-mode "Inner Glow" glow-type)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
    "script-fu-layer-effects-inner-glow"
    "<Image>/FX-Foundry/Layer Effects/Inner Glow..."
    "Create the Inner Glow on the layer with alpha"
    "Iccii "
    "Iccii"
    "Aug, 2001"
    "RGBA"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-COLOR "Glow Color" '(255 255 191)
    SF-ADJUSTMENT "Glow Radius" '(2 0 100 1 10 0 1)
    SF-ADJUSTMENT "Blur Radius" '(10 0 100 1 10 0 1)
    SF-ADJUSTMENT "Opacity" '(75 0 100 1 10 0 0)
    SF-OPTION "Glow Layer Mode" '("Default (Screen)" "Dissolve" "Normal"
    "Multiply" "Screen" "Overlay" "Difference"
    "Addition" "Subtract" "Darken" "Lighten"
    "Hue" "Saturation" "Color" "Value" "Divide")
    SF-OPTION "Glow Type" '("Edge" "Inner")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bevel and Emboss script ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-layer-effects-bevel-and-emboss
            img ;
            layer ;
            effect-style ; Outer Bevel(0) Inner Bevel(1)
            ; Emboss(2) Pillow Emboss(3)
            angle ;
            depth ;
            blur-radius ;
            light-color ;
            light-opacity ;
            light-mode ;
            shadow-color ;
            shadow-opacity;
            shadow-mode ;
        )

(let* (
        (old-bg (car (gimp-context-get-background)))
        (light-mode (cond ((= light-mode 0) 4)
        ((= light-mode 2) 0)
        (light-mode)))
        (shadow-mode (cond ((= shadow-mode 0) 3)
        ((= shadow-mode 2) 0)
        (shadow-mode)))
      )

        (gimp-image-undo-group-start img)
        (cond
        ((= effect-style 0) ; Inner Bevel
            (layer-effects-common1 img layer shadow-color angle depth 0 blur-radius
            shadow-opacity shadow-mode "Bevel Shadow" 0)
            (layer-effects-common1 img layer light-color (+ angle 180) depth 0 blur-radius
            light-opacity light-mode "Bevel Light" 0)
        )
            ((= effect-style 1) ; Outer Bevel
            (layer-effects-common1 img layer shadow-color (+ angle 180) depth 0 blur-radius
            shadow-opacity shadow-mode "Bevel Shadow" 1)
            (layer-effects-common1 img layer light-color angle depth 0 blur-radius
            light-opacity light-mode "Bevel Light" 1)
        )
        ((= effect-style 2) ; Emboss
            (layer-effects-common1 img layer shadow-color angle depth 0 blur-radius
            shadow-opacity shadow-mode "Enboss Outer Shadow" 0)
            (layer-effects-common1 img layer light-color (+ angle 180) depth 0 blur-radius
            light-opacity light-mode "Emboss Outer Light" 0)
            (layer-effects-common1 img layer shadow-color (+ angle 180) depth 0 blur-radius
            shadow-opacity shadow-mode "Emboss Inner Shadow" 1)
            (layer-effects-common1 img layer light-color angle depth 0 blur-radius
            light-opacity light-mode "Emboss Inner Light" 1)
        )
        ((= effect-style 3) ; Pillow Emboss
            (layer-effects-common1 img layer shadow-color (+ angle 180) depth 0 blur-radius
            shadow-opacity shadow-mode "Enboss Outer Shadow" 0)
            (layer-effects-common1 img layer light-color angle depth 0 blur-radius
            light-opacity light-mode "Emboss Outer Light" 0)
            (layer-effects-common1 img layer shadow-color (+ angle 180) depth 0 blur-radius
            shadow-opacity shadow-mode "Emboss Inner Shadow" 1)
            (layer-effects-common1 img layer light-color angle depth 0 blur-radius
            light-opacity light-mode "Emboss Inner Light" 1)
        )
        ) ; end of cond
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
    )
)

(script-fu-register
    "script-fu-layer-effects-bevel-and-emboss"
    "<Image>/FX-Foundry/Layer Effects/Bevel and Emboss..."
    "Create the Bevel and Emboss effects on the layer with alpha"
    "Iccii "
    "Iccii"
    "Aug, 2001"
    "RGBA"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-OPTION "Effect Style" '("Outer Bevel" "Inner Bevel"
    "Emboss" "Pillow Emboss")
    SF-ADJUSTMENT "Lighting (degrees)" '(30 0 360 1 15 0 0)
    SF-ADJUSTMENT "Depth" '(5 0 100 1 10 0 1)
    SF-ADJUSTMENT "Blur Radius" '(5 0 100 1 10 0 1)
    SF-COLOR "Highlight Color" '(255 255 255)
    SF-ADJUSTMENT "Highlight Opacity" '(75 0 100 1 10 0 0)
    SF-OPTION "Highlight Layer Mode" '("Default (Screen)" "Dissolve" "Normal"
    "Multiply" "Screen" "Overlay" "Difference"
    "Addition" "Subtract" "Darken" "Lighten"
    "Hue" "Saturation" "Color" "Value" "Divide")
    SF-COLOR "Shadow Color" '(0 0 0)
    SF-ADJUSTMENT "Shadow Opacity" '(75 0 100 1 10 0 0)
    SF-OPTION "Shadow Layer Mode" '("Default (Multiply)" "Dissolve" "Normal"
    "Multiply" "Screen" "Overlay" "Difference"
    "Addition" "Subtract" "Darken" "Lighten"
    "Hue" "Saturation" "Color" "Value" "Divide")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Satin Effect script ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (script-fu-layer-effects-satin
            img ;
            layer ;
            color ;
            angle ;
            offset-radius ;
            blur-radius ;
            opacity ;
            layer-mode ;
            invert? ;
        )

(let* (
        (old-bg (car (gimp-context-get-background)))
        (width (car (gimp-drawable-width layer)))
        (height (car (gimp-drawable-height layer)))
        (radians (/ (* 2 *pi* angle) 360))
        (x-offset (* offset-radius (sin radians)))
        (y-offset (* offset-radius (cos radians)))
        (layer-mode (cond ((= layer-mode 0) 3)
        ((= layer-mode 2) 0)
        (layer-mode)))
        (satin-layer1 (car (gimp-layer-new img width height RGBA-IMAGE
        "Satin Effect" 100 NORMAL-MODE)))
        (satin-layer2 (car (gimp-layer-new img width height RGBA-IMAGE
        "Satin Effect" 100 DIFFERENCE-MODE)))
        (satin-layer)
        (satin-mask1)
        (satin-mask2)
        (satin-mask3)
      )

        (gimp-image-undo-group-start img)
        (gimp-image-add-layer img satin-layer1 -1)
        (gimp-image-add-layer img satin-layer2 -1)
        (gimp-drawable-fill satin-layer1 WHITE-IMAGE-FILL)
        (gimp-drawable-fill satin-layer2 WHITE-IMAGE-FILL)
        (gimp-selection-layer-alpha layer)
        (gimp-context-set-background '(0 0 0))
        (gimp-edit-fill satin-layer1 BG-IMAGE-FILL)
        (gimp-edit-fill satin-layer2 BG-IMAGE-FILL)
        (gimp-drawable-offset satin-layer1 FALSE OFFSET-BACKGROUND x-offset y-offset)
        (gimp-drawable-offset satin-layer2 FALSE OFFSET-BACKGROUND (- x-offset) (- y-offset))

        (set! satin-layer (car (gimp-image-merge-down img satin-layer2 EXPAND-AS-NECESSARY)))
        (set! satin-mask1 (car (gimp-layer-create-mask satin-layer BLACK-MASK)))
        ;replaced 2005-11-13
        ; (gimp-image-add-layer-mask img satin-layer satin-mask1)
        (gimp-layer-add-mask satin-layer satin-mask1)
        (gimp-edit-fill satin-mask1 WHITE-IMAGE-FILL)
        (gimp-selection-none img)
        ;(plug-in-colortoalpha 1 img satin-layer '(255 255 255))
        (if (eqv? invert? TRUE)
        (gimp-invert satin-layer))
        (gimp-edit-copy satin-layer)
        (set! satin-mask2 (car (gimp-floating-sel-anchor
        (car (gimp-edit-paste satin-mask1 0)))))
        ;replaced 2005-11-13
        ; (gimp-image-remove-layer-mask img satin-layer APPLY)
        (gimp-layer-remove-mask satin-layer APPLY)
        (set! satin-mask3 (car (gimp-layer-create-mask satin-layer BLACK-MASK)))
        ;replaced 2005-11-13
        ; (gimp-image-add-layer-mask img satin-layer satin-mask3)
        (gimp-layer-add-mask satin-layer satin-mask3)
        (gimp-selection-layer-alpha layer)
        (gimp-edit-fill satin-mask3 WHITE-IMAGE-FILL)

        (gimp-selection-layer-alpha satin-layer)
        (gimp-context-set-background color)
        (gimp-edit-fill satin-layer BG-IMAGE-FILL)
        (gimp-selection-none img)
        (if (< 0 blur-radius)
        (plug-in-gauss-iir2 1 img satin-layer blur-radius blur-radius))
        (gimp-layer-set-opacity satin-layer opacity)
        (gimp-layer-set-mode satin-layer layer-mode)

        (gimp-image-set-active-layer img layer)
        (gimp-context-set-background old-bg)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        (list satin-layer)
    )
)

(script-fu-register
    "script-fu-layer-effects-satin"
    "<Image>/FX-Foundry/Layer Effects/Satin..."
    "Create the Satin effect on the layer with alpha"
    "Iccii "
    "Iccii"
    "Aug, 2001"
    "RGBA"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-COLOR "Satin Color" '(0 0 0)
    SF-ADJUSTMENT "Angle" '(30 0 360 1 15 0 0)
    SF-ADJUSTMENT "Satin Radius" '(10 0 100 1 10 0 1)
    SF-ADJUSTMENT "Blur Radius" '(10 0 100 1 10 0 1)
    SF-ADJUSTMENT "Opacity" '(50 0 100 1 10 0 0)
    SF-OPTION "Satin Layer Mode" '("Default (Multiply)" "Dissolve" "Normal"
    "Multiply" "Screen" "Overlay" "Difference"
    "Addition" "Subtract" "Darken" "Lighten"
    "Hue" "Saturation" "Color" "Value" "Divide")
    SF-TOGGLE "Invert" TRUE
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color Overlay script ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (script-fu-layer-effects-color-overlay
    img ;
    layer ;
    color ;
    opacity ;
    layer-mode ;
)

(let* (
        (old-bg (car (gimp-context-get-background)))
        (width (car (gimp-drawable-width layer)))
        (height (car (gimp-drawable-height layer)))
        (layer-mode (cond ((= layer-mode 0) 0)
        ((= layer-mode 2) 0)
        (layer-mode)))
        (color-layer (car (gimp-layer-new img width height RGBA-IMAGE
        "Color Fill Layer" opacity layer-mode)))
        )

        (gimp-image-undo-group-start img)
        (gimp-image-add-layer img color-layer -1)
        (gimp-drawable-fill color-layer TRANS-IMAGE-FILL)
        (gimp-selection-layer-alpha layer)
        (gimp-context-set-background color)
        (gimp-edit-fill color-layer BG-IMAGE-FILL)
        (gimp-selection-none img)

        (gimp-image-set-active-layer img layer)
        (gimp-context-set-background old-bg)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        (list color-layer)
      )
)

(script-fu-register
    "script-fu-layer-effects-color-overlay"
    "<Image>/FX-Foundry/Layer Effects/Color Overlay..."
    "Create the Color overlay effect on the layer with alpha"
    "Iccii "
    "Iccii"
    "Aug, 2001"
    "RGBA"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-COLOR "Color" '(0 0 0)
    SF-ADJUSTMENT "Opacity" '(100 0 100 1 10 0 0)
    SF-OPTION "Color Fill Mode" '("Default (Normal)" "Dissolve" "Normal"
    "Multiply" "Screen" "Overlay" "Difference"
    "Addition" "Subtract" "Darken" "Lighten"
    "Hue" "Saturation" "Color" "Value" "Divide")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gradient Overlay script ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (script-fu-layer-effects-gradient-overlay
            img ;
            layer ;
            fg-color ;
            bg-color ;
            blend-mode ;
            gradient ;
            angle ;
            scale ;
            opacity ;
            layer-mode ;
            style ;
            repeat ;
         )

  (let* (
        (old-fg (car (gimp-context-get-foreground)))
        (old-bg (car (gimp-context-get-background)))
        (old-gradient (car (gimp-context-get-gradient)))
        (width (car (gimp-drawable-width layer)))
        (height (car (gimp-drawable-height layer)))
        (layer-mode (cond ((= layer-mode 0) 0)
        ((= layer-mode 2) 0)
        (layer-mode)))
        (radians (/ (* 2 *pi* angle) 360))
        (x-distance (* 0.5 scale width (sin radians)))
        (y-distance (* 0.5 scale height (cos radians)))
        (x-center (/ width 2))
        (y-center (/ height 2))
        (x1 (- x-center x-distance))
        (y1 (- y-center y-distance))
        (x2 (+ x-center x-distance))
        (y2 (+ y-center y-distance))
        (gradient-layer (car (gimp-layer-new img width height RGBA-IMAGE
        "Gradient Fill Layer" opacity layer-mode)))
      )

        (gimp-image-undo-group-start img)
        (gimp-image-add-layer img gradient-layer -1)
        (gimp-drawable-fill gradient-layer TRANS-IMAGE-FILL)
        (gimp-selection-layer-alpha layer)
        (gimp-context-set-foreground fg-color)
        (gimp-context-set-background bg-color)
        (gimp-context-set-gradient gradient)
        ;
        ;hier werden 14 Parameter übergeben, 16 sind aber gefordert
        ;da der Programmierer wie es scheint supersampling nicht beachtet hat,
        ;setze ich das auf FALSE und den fehlenden supersamling threshold auf 0
        ;(sollte durch das FALSE egal sein, welchen Wert das hat)
        ;ausserdem hab' ich auch hier die deprecated procedure ersetzt
        ;
        ; (gimp-edit-blend gradient-layer blend-mode NORMAL-MODE style 100 0 repeat
        ; FALSE 0 0 x1 y1 x2 y2)
        ;
        (gimp-edit-blend gradient-layer blend-mode NORMAL-MODE style 100 0 repeat
        FALSE FALSE 0 0 0 x1 y1 x2 y2)
        (gimp-selection-none img)

        (gimp-image-set-active-layer img layer)
        (gimp-context-set-foreground old-fg)
        (gimp-context-set-background old-bg)
        (gimp-context-set-gradient old-gradient)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        (list gradient-layer)
  )
)

(script-fu-register
    "script-fu-layer-effects-gradient-overlay"
    "<Image>/FX-Foundry/Layer Effects/Gradient Overlay..."
    "Create the Gradient overlay effect on the layer with alpha"
    "Iccii "
    "Iccii"
    "Aug, 2001"
    "RGBA"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-COLOR "Foreground Color" '(127 255 255)
    SF-COLOR "Background Color" '(127 255 127)
    SF-OPTION "Blend Mode" '("FG-BG RGB" "FG-BG HSV" "FG-Trans" "Custom")
    SF-GRADIENT "Gradient" "Sunrise"
    SF-ADJUSTMENT "Angle" '(30 0 360 1 15 0 0)
    SF-ADJUSTMENT "Scale" '(1.00 0.05 5 0.05 0.1 2 0)
    SF-ADJUSTMENT "Opacity" '(100 0 100 1 10 0 0)
    SF-OPTION "Gradient Fill Mode" '("Default (Normal)" "Dissolve" "Normal"
    "Multiply" "Screen" "Overlay" "Difference"
    "Addition" "Subtract" "Darken" "Lighten"
    "Hue" "Saturation" "Color" "Value" "Divide")
    SF-OPTION "Gradient Style" '("Linear" "Biliner" "Radial" "Square"
    "Conical Symmetric" "Conical Asymmetric"
    "Shapeburst Angular" "Shapeburst Spherical"
    "Shapeburst Dimpled" "Spiral Clockwise"
    "Spiral Anticlockwise")
    SF-OPTION "Repeat" '("None" "Sawtooth Wave" "Triangular Wave")
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern Overlay script ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (script-fu-layer-effects-pattern-overlay
    img ;
    layer ;
    pattern ;
    opacity ;
    layer-mode ;
)

    (let* (
            (old-pattern (car (gimp-context-get-pattern)))
            (width (car (gimp-drawable-width layer)))
            (height (car (gimp-drawable-height layer)))
            (layer-mode (cond ((= layer-mode 0) 0)
            ((= layer-mode 2) 0)
            (layer-mode)))
            (pattern-layer (car (gimp-layer-new img width height RGBA-IMAGE
            "Pattern Fill Layer" opacity layer-mode)))
          )

        (gimp-image-undo-group-start img)
        (gimp-image-add-layer img pattern-layer -1)
        (gimp-drawable-fill pattern-layer TRANS-IMAGE-FILL)
        (gimp-selection-layer-alpha layer)
        (gimp-context-set-pattern pattern)
        (gimp-edit-bucket-fill pattern-layer PATTERN-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
        (gimp-selection-none img)

        (gimp-image-set-active-layer img layer)
        (gimp-context-set-pattern old-pattern)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        (list pattern-layer)
    )
)

(script-fu-register
    "script-fu-layer-effects-pattern-overlay"
    "<Image>/FX-Foundry/Layer Effects/Pattern Overlay..."
    "Create the Pattern overlay effect on the layer with alpha"
    "Iccii "
    "Iccii"
    "Aug, 2001"
    "RGBA"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-PATTERN "Pattern" "Pine?"
    SF-ADJUSTMENT "Opacity" '(100 0 100 1 10 0 0)
    SF-OPTION "Pattern Fill Mode" '("Default (Normal)" "Dissolve" "Normal"
    "Multiply" "Screen" "Overlay" "Difference"
    "Addition" "Subtract" "Darken" "Lighten"
    "Hue" "Saturation" "Color" "Value" "Divide")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add Border script ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FIXME: Outside and center result in filled layer for no alpha image.
(define (script-fu-layer-effects-add-border
            img ;
            layer ;
            type ; Color(0) Gradient(1) Pattern(2)
            size ;
            position ;
            opacity ;
            layer-mode ;
            color ;
            fg-color ;
            bg-color ;
            blend-mode ;
            gradient ;
            angle ;
            scale ;
            style ;
            repeat ;
            pattern ;
            antialias? ;
        )

    (let* (
            (width (car (gimp-drawable-width layer)))
            (height (car (gimp-drawable-height layer)))
            (border-layer (car (gimp-layer-new img width height RGBA-IMAGE
            "Boder Line" opacity layer-mode)))
            (result-layer)
          )

        (gimp-image-undo-group-start img)
        (gimp-image-add-layer img border-layer -1)
        (gimp-drawable-fill border-layer TRANS-IMAGE-FILL)
        (gimp-selection-layer-alpha layer)
        (cond
            ((= position 0)
            (gimp-selection-shrink img (- size 3)))
            ((= position 1)
            (gimp-selection-grow img (- size 2)))
        ) ; end of cond
        (if (< (car (gimp-selection-is-empty img)) 0)
        (gimp-message "The selection is empty. Abort.")
            (begin
                (gimp-selection-border img size)
                (gimp-selection-sharpen img)
                (gimp-edit-fill border-layer WHITE-IMAGE-FILL)
                (if (= antialias? TRUE)
                    (begin
                    (gimp-layer-scale border-layer (* 2 width) (* 2 height) 0)
                    (gimp-layer-scale border-layer width height 0)))
                (set! result-layer (car
                (cond
                    ((= type 0)
                    (script-fu-layer-effects-color-overlay
                    img border-layer color opacity layer-mode))
                    ((= type 1)
                    (script-fu-layer-effects-gradient-overlay
                    img border-layer fg-color bg-color blend-mode gradient
                    angle scale opacity layer-mode style repeat))
                    ((= type 2)
                    (script-fu-layer-effects-pattern-overlay
                    img border-layer pattern opacity layer-mode))
                ) ; end of cond
                )) ; end of set! and car
                (gimp-drawable-fill border-layer TRANS-IMAGE-FILL)
                (set! border-layer (car (gimp-image-merge-down
                img result-layer EXPAND-AS-NECESSARY)))
            ) ; end of begin
        ) ; end of if

        (gimp-image-set-active-layer img layer)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        (list border-layer)
    )
)

(script-fu-register
    "script-fu-layer-effects-add-border"
    "<Image>/FX-Foundry/Layer Effects/Add Border..."
    "Create the Add Border effect on the layer with alpha"
    "Iccii "
    "Iccii"
    "Aug, 2001"
    "RGBA"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-OPTION "Border Fill Type" '("Color" "Gradient" "Pattern")
    SF-ADJUSTMENT "Border Size" '(5 3 100 1 10 0 1)
    SF-OPTION "Border Position" '("Inner Border" "Outer Border" "Center Border")
    SF-ADJUSTMENT "Opacity" '(100 0 100 1 10 0 0)
    SF-OPTION "Border Fill Mode" '("Default (Normal)" "Dissolve" "Normal"
    "Multiply" "Screen" "Overlay" "Difference"
    "Addition" "Subtract" "Darken" "Lighten"
    "Hue" "Saturation" "Color" "Value" "Divide")
    SF-COLOR "Color" '(0 0 0)
    SF-COLOR "Foreground Color" '(127 255 255)
    SF-COLOR "Background Color" '(127 255 127)
    SF-OPTION "Blend Mode" '("FG-BG RGB" "FG-BG HSV" "FG-Trans" "Custom")
    SF-GRADIENT "Gradient" "Sunrise"
    SF-ADJUSTMENT "Angle" '(30 0 360 1 15 0 0)
    SF-ADJUSTMENT "Scale" '(1.00 0.05 5 0.05 0.1 2 0)
    SF-OPTION "Gradient Style" '("Linear" "Biliner" "Radial" "Square"
    "Conical Symmetric" "Conical Asymmetric"
    "Shapeburst Angular" "Shapeburst Spherical"
    "Shapeburst Dimpled" "Spiral Clockwise"
    "Spiral Anticlockwise")
    SF-OPTION "Repeat" '("None" "Sawtooth Wave" "Triangular Wave")
    SF-PATTERN "Pattern" "Pine?"
    SF-TOGGLE "Antialias" TRUE
)



;; Version de abcdugimp.free.fr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layer Style sample script ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-layer-effects-styles-sample
    img ;
    layer ;
    angle ;
    pattern ;
)

    (let* (
            (old-bg (car (gimp-palette-get-background)))
          )

            (gimp-undo-push-group-start img)
            (script-fu-layer-effects-bevel-and-emboss img layer 2 angle 10 10
            '(255 255 255) 75 4
            '( 0 0 0) 75 3)
            (script-fu-layer-effects-gradient-overlay img layer '(255 255 255) '(214 214 214)
            0 "Default" angle 1.0 25 4 0 0)
            (script-fu-layer-effects-pattern-overlay img layer pattern 100 0)
            (gimp-undo-push-group-end img)
            (gimp-displays-flush)
    )
)
(script-fu-register
    "script-fu-layer-effects-styles-sample"
    "<Image>/FX-Foundry/Layer Effects/Style Sample..."
    "Layer Style sample script."
    "Iccii "
    "Iccii"
    "Aug, 2001"
    "RGBA"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-ADJUSTMENT "Angle (Degree)" '(30 0 360 1 10 0 0)
    SF-PATTERN "Pattern" "Pine?"
)

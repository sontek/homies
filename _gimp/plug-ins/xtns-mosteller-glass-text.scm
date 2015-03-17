;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Glass Effect script  for GIMP 2.4
; Created by Scott Mostelle
;
; Tags: text, logo, glass, new image
;
; Author statement:
;
; Simply enter the text and select the effect type. The scrip will generate 4 frames with the selected effect
; You can control various aspects of each effect by adjusting the effect tweak parameter and the scatter
; parameter.
;
; Simply save the generated text as an animated .GIF and you're done.
;
; Selecting Raw Layer will display all raw layers only
; Clicking off animation will display a single frame, flattend to background.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;
; --------------------------------------------------------------------
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; User Options Popup
;
(script-fu-register "script-fu-glass-effect-text"
            _"_GlassEffect Text..."
            "Creates Glass-Effect Text with a drop shadow"
            "Scott Mosteller"
            "Scott Mosteller"
            "2007"
            ""
            SF-STRING     _"Text"               "Gimp"
            SF-ADJUSTMENT _"Font size (pixels)" '(150 2 1000 1 10 0 1)
            SF-FONT       _"Font"               "Arial Black"
            SF-COLOR      _"Text color"         '(123 149 176)
                    SF-PATTERN    _"Background Pattern" "Dried mud"
                    SF-ADJUSTMENT _"Glass Depth"        '(3 1 10 1 1 0 1)
                    SF-ADJUSTMENT _"Glass Translucency" '(64 0 255 1 1 0 1)
            SF-COLOR      _"Shadow color"       '(0 0 0)
                    SF-ADJUSTMENT _"Shadow Offset X"    '(12 -25 25 1 1 0 1)
                    SF-ADJUSTMENT _"Shadow Offset Y"    '(12 -25 25 1 1 0 1)
                    SF-ADJUSTMENT _"Shadow Blur"        '(8 0 25 1 1 0 1)
                    SF-ADJUSTMENT _"Shadow Opacity"     '(60 0 100 1 1 0 1)
                    SF-TOGGLE     _"Include Shadow?"     TRUE
                    SF-TOGGLE     _"Include Background"  TRUE)
;
; Register on Menu
;
(script-fu-menu-register "script-fu-glass-effect-text"
             _"<Toolbox>/Xtns/FX-Foundry/Logos")
;
; Define Function For Glass Translucency Values
;
(define (get-glass-trans-curve parm)
  (let* ((curve-value (cons-array 4 'byte)))
   (aset curve-value 0 0)
   (aset curve-value 1 0)
   (aset curve-value 2 255)
   (aset curve-value 3 parm)
   curve-value
   )
)
;
; Define Main Glass Text Function
;
(define (script-fu-glass-effect-text text
                   size
                   font
                   text-color
                               lpat
                   glass-depth
                               glass-trans
                               shadow-color
                               shx
                               shy
                               shb
                               sho
                               dsh
                               bkg)
  (let* ((img (car (gimp-image-new 256 256 RGB)))
    (tmp (car (gimp-context-set-foreground '( 255 255 255))))
    (text-layer2 (car (gimp-text-fontname img -1 0 0 text 10 TRUE (+ size shx) PIXELS font)))
    (text-layer3 (car (gimp-text-fontname img -1 0 0 text 10 TRUE size PIXELS font)))
    (tmp (car (gimp-context-set-foreground text-color)))
    (text-layer4 (car (gimp-text-fontname img -1 0 0 text 10 TRUE size PIXELS font)))
    (glass-layer 0)
    (trns 0)
    (spvalues (cons-array 4 'byte)))

;
; Glass Text Main Procedure Body
;
   (gimp-image-undo-disable img)
   (gimp-image-resize-to-layers img)
;
; Fill background
;
   (gimp-image-set-active-layer img text-layer2)
   (gimp-context-set-pattern lpat)
   (gimp-drawable-fill text-layer2 4)
   (gimp-drawable-set-name text-layer2 "Background")
   (gimp-layer-resize-to-image-size text-layer2)
;
; Create Glass Text
;
;
   (gimp-image-set-active-layer img text-layer3)
   (gimp-layer-resize-to-image-size text-layer3)
   (gimp-selection-layer-alpha text-layer3)
   (plug-in-gauss 1 img text-layer3 5 5 1)
   (gimp-drawable-set-visible text-layer3 0)

   (gimp-image-set-active-layer img text-layer4)
   (gimp-layer-resize-to-image-size text-layer4)
   (gimp-invert text-layer4)
   (plug-in-bump-map 1 img text-layer4 text-layer3 135 45 (+ glass-depth 0) 0 0 0 0 0 0 0)
   (gimp-selection-shrink img glass-depth)
   (gimp-selection-feather img (- glass-depth 1))

   (gimp-curves-spline text-layer4 4 4 (get-glass-trans-curve glass-trans))

   (set! glass-layer (car (gimp-layer-copy text-layer4 1)))
   (gimp-image-add-layer img glass-layer -1)

   (gimp-edit-clear text-layer4)
   (gimp-selection-none img)
   (gimp-hue-saturation text-layer4 0 0 0 -100)

   (gimp-invert glass-layer)
;
; Create shadow layer on request
;

  (if (= dsh TRUE)
   (begin
    (script-fu-drop-shadow img text-layer4 shx shy shb shadow-color sho TRUE)
   ))
;
; Clean up & delete layers as needed
;
  (gimp-image-remove-layer img text-layer3)
  (gimp-image-remove-layer img text-layer4)
  (gimp-image-set-active-layer img text-layer2)

  (if (= bkg FALSE)
   (begin
   (gimp-image-remove-layer img text-layer2)
   ))

   (gimp-selection-none img)
   (gimp-layer-resize-to-image-size glass-layer)
   (gimp-image-undo-enable img)
   (gimp-display-new img)))
;
; End Glass Text Main Procedure
;

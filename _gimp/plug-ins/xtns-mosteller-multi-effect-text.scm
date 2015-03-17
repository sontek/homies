;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Multi-Text Effect Script for GIMP 2.4
; Created by Scott Mosteller
;
; Tags: logo
;
; Author statement:
;
; Comments directed to http://www.gimpscripts.com
;
; Create text with a choice of multiple effects
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
; *NOTE: Scatter-HSV is handled differently 2.4 that it was in Gimp 2.2.
;        When using Gimp 2.2, set scatter lower (40-60)
;        When using Gimp 2.4, set scatter higher (140-160)
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; V1.0 - Initial Release
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

;
;
; User Options Popup
;
(script-fu-register "script-fu-multi-effect-text"
            _"_MultiEffect Text..."
            "Creates a Multi-Effect Text with a drop shadow"
            "Scott Mosteller"
            "Scott Mosteller"
            "2007"
            ""
            SF-STRING     _"Text"               "Gimp"
            SF-ADJUSTMENT _"Font size (pixels)" '(180 2 1000 1 10 0 1)
            SF-FONT       _"Font"               "Arial Bold"
            SF-COLOR      _"Text color"         '(195 8 8)
            SF-COLOR      _"Stroke color color" '(0 0 0)
                    SF-ADJUSTMENT _"Stroke width"       '(2 1 50 1 1 0 1)
            SF-COLOR      _"Shadow color"       '(0 0 0)
            SF-COLOR      _"Background color"   '(255 255 255)
                    SF-ADJUSTMENT _"Shadow Offset X"    '(4 -99 99 1 1 0 1)
                    SF-ADJUSTMENT _"Shadow Offset Y"    '(4 -99 99 1 1 0 1)
                    SF-ADJUSTMENT _"Shadow Opacity"     '(60 1 100 1 1 0 1)
                    SF-OPTION     _"Effect Type"       '("Cubism" "Oilify" "Plasma" "Diffraction" "No Effect")
                    SF-ADJUSTMENT _"Effect Tweak"       '(4.5 1 20 .1 .1 1 0)
                    SF-ADJUSTMENT _"Scatter Amount"     '(150 1 180 1 1 0 0)
                    SF-TOGGLE     _"Animate?"           TRUE
                    SF-TOGGLE     _"Raw Layers Only?"   FALSE)
;
; Register on Menu
;
(script-fu-menu-register "script-fu-multi-effect-text"
             _"<Toolbox>/Xtns/FX-Foundry/Logos")
;
; Define Main Multi-effect Text Function
;
(define (script-fu-multi-effect-text text
                   size
                   font
                   text-color
                   stroke-color
                               stroke-width
                               shadow-color
                   bg-color
                               shx
                               shy
                               sho
                               etype
                               tweak
                               scatter
                               lani
                               raw)
  (let* ((img (car (gimp-image-new 256 256 RGB)))
    (tmpfg (car (gimp-context-get-foreground img)))
    (tmpbg (car (gimp-context-get-background img)))
    (tmp (car (gimp-context-set-foreground stroke-color)))
    (text-layer (car (gimp-text-fontname img -1 0 0 text 10 TRUE size PIXELS font)))
    (text-layer2 (car (gimp-text-fontname img -1 0 0 text 10 TRUE size PIXELS font)))
    (text-layer3 (car (gimp-text-fontname img -1 0 0 text 10 TRUE size PIXELS font)))
    (text-layer4 (car (gimp-text-fontname img -1 0 0 text 10 TRUE size PIXELS font)))
    (lay1 0)
    (lay2 0)
    (lay3 0)
    (lay4 0)
    (diffinc 0))
;
; Multi-effect  Text Main Procedure Body
;
;
; Turn off text layers 2,3,4
;
    (gimp-drawable-set-visible text-layer2 0)
    (gimp-drawable-set-visible text-layer3 0)
    (gimp-drawable-set-visible text-layer4 0)
;
; Create animation layer 1
;
(set! diffinc (- tweak 1))
(gimp-image-undo-disable img)
(gimp-drawable-set-name text-layer text)
(apply-multi-effect-text img text-layer text-color stroke-color stroke-width shadow-color bg-color shx shy sho etype tweak scatter lani raw tmpfg tmpbg diffinc)
(if (= raw FALSE)
  (begin
   (set! lay1 (car (gimp-image-merge-visible-layers img 0)))
   (gimp-drawable-set-name lay1 "frame1")
   (gimp-selection-none img)
   (gimp-context-set-background bg-color)
   (gimp-image-set-active-layer img lay1)
   (plug-in-semiflatten 1 img lay1)
   (gimp-drawable-set-visible lay1 0)))
;
; Skip if not animated
;
(if (= lani TRUE)
  (begin
;
; Create animation layer 2
;
(set! diffinc (- (+ tweak 0.35) 1))
(gimp-drawable-set-visible text-layer2 1)
(apply-multi-effect-text img text-layer2 text-color stroke-color stroke-width shadow-color bg-color shx shy sho etype tweak scatter lani raw tmpfg tmpbg diffinc)
(if (= raw FALSE)
  (begin
   (set! lay2 (car (gimp-image-merge-visible-layers img 0)))
   (gimp-drawable-set-name lay2 "frame2")
   (gimp-selection-none img)
   (gimp-context-set-background bg-color)
   (gimp-image-set-active-layer img lay2)
   (plug-in-semiflatten 1 img lay2)
   (gimp-drawable-set-visible lay2 0)))
;
; Create animation layer 3
;
(set! diffinc (- (+ tweak 0.7) 1))
(gimp-drawable-set-visible text-layer3 1)
(apply-multi-effect-text img text-layer3 text-color stroke-color stroke-width shadow-color bg-color shx shy sho etype tweak scatter lani raw tmpfg tmpbg diffinc)
(if (= raw FALSE)
  (begin
   (set! lay3 (car (gimp-image-merge-visible-layers img 0)))
   (gimp-drawable-set-name lay3 "frame3")
   (gimp-selection-none img)
   (gimp-context-set-background bg-color)
   (gimp-image-set-active-layer img lay3)
   (plug-in-semiflatten 1 img lay3)
   (gimp-drawable-set-visible lay3 0)))
;
; Create animation layer 4
;
(set! diffinc (- (+ tweak 1.05) 1))
(gimp-drawable-set-visible text-layer4 1)
(apply-multi-effect-text img text-layer4 text-color stroke-color stroke-width shadow-color bg-color shx shy sho etype tweak scatter lani raw tmpfg tmpbg diffinc)
(if (= raw FALSE)
  (begin
    (set! lay4 (car (gimp-image-merge-visible-layers img 0)))
    (gimp-drawable-set-name lay4 "frame4")
    (gimp-selection-none img)
    (gimp-context-set-background bg-color)
    (gimp-image-set-active-layer img lay4)
    (plug-in-semiflatten 1 img lay4)
    (gimp-drawable-set-visible lay4 0)))
))

;
; Raw and animation cleanup
;
(if (= raw FALSE)
  (begin
    (gimp-drawable-set-visible lay1 1)
    (plug-in-autocrop 1 img lay1)
     (if (= lani TRUE)
       (begin
         (gimp-drawable-set-visible lay2 1)
         (gimp-drawable-set-visible lay3 1)
         (gimp-drawable-set-visible lay4 1)
        )
        (begin
         (gimp-selection-none img)
         (gimp-image-raise-layer-to-top img lay1)
         (gimp-image-flatten img)))
 ))
;
; Clear selection, enable undo and display image
;
    (gimp-selection-none img)
    (gimp-image-undo-enable img)
    (gimp-display-new img)))
;
; End Multi-effect  Text Main Procedure
;
;
; Define Multi-effect Text Apply Routine
;
;
(define (apply-multi-effect-text img
                  logo-layer
                  text-color
                                  stroke-color
                                  stroke-width
                                  shadow-color
                  bg-color
                                  shx
                                  shy
                                  sho
                                  etype
                                  tweak
                                  scatter
                                  lani
                                  raw
                                  tmpfg
                                  tmpbg
                                  diffinc)
  (let* ((width (car (gimp-drawable-width logo-layer)))
     (height (car (gimp-drawable-height logo-layer)))
     (shadow-layer (car (gimp-layer-new img width height RGBA-IMAGE "Shadow" sho NORMAL-MODE)))
         (df1 0)
         (df2 0)
         (df3 0)
         (df4 0)
         (df5 0)
         (df6 0)
     )
;
; Subroutine - Multi-effect  Text Apply Body
;

   (gimp-selection-none img)
   (script-fu-util-image-resize-from-layer img logo-layer)

;
; Set color for active gradient & call effects procedure
;
;   (gimp-context-set-foreground tmpfg)
;   (gimp-context-set-background tmpbg)


;
; Fill text layer with foreground color
;
  (gimp-by-color-select logo-layer stroke-color 15 2 TRUE FALSE 10 FALSE)
  (gimp-selection-shrink img stroke-width)
  (gimp-context-set-foreground text-color)
  (gimp-edit-bucket-fill logo-layer 0 0 100 0 0 0 0)

;  (script-fu-add-bevel img logo-layer 20 0 0)
;
;
; Effect #1 - Cubism
;
(if (= etype 0)
   (begin
   (plug-in-scatter-hsv 1 img logo-layer 1 scatter 10 10)
   (plug-in-cubism 1 img logo-layer tweak 2 0)
   ))
;
; Effect #2 - Oilify
;
(if (= etype 1)
   (begin
   (plug-in-scatter-hsv 1 img logo-layer 1 scatter 10 10)
   (plug-in-oilify 1 img logo-layer tweak 0)
   ))
;
; Effect #2 - Plasma
;
(if (= etype 2)
   (begin
   (plug-in-scatter-hsv 1 img logo-layer 1 scatter 10 10)
   (plug-in-plasma 1 img logo-layer (rand 999999999) tweak)
   ))
;
; Effect #3 - Diffraction
;
;

(if (= etype 3)
   (begin
   (set! df1 (+ .821 diffinc)) ;Calculate parameters using tweak value
   (set! df2 (+ 1.221 diffinc))
   (set! df3 (+ 1.123 diffinc))
   (set! df4 (+ .821 (/ diffinc 2)))
   (set! df5 (+ .821 (/ diffinc 2)))
   (set! df6 (+ .974 (/ diffinc 2)))
   (plug-in-diffraction 1 img logo-layer df1 df2 df3 df4 df5 df6 0.610 0.677 0.636 0.066 37.126 -0.473)
   ))
;
; Create shadow layer
;
   (gimp-selection-none img)
   (gimp-image-add-layer img shadow-layer 1)
   (gimp-layer-resize-to-image-size shadow-layer)
   (gimp-edit-clear shadow-layer)
   (gimp-selection-layer-alpha logo-layer)
   (gimp-selection-feather img 5)
   (gimp-context-set-background shadow-color)
   (gimp-edit-fill shadow-layer BACKGROUND-FILL)
   (gimp-layer-translate shadow-layer shx shy)

   (gimp-image-lower-layer img shadow-layer) ; Drop shadow to bottom
   (gimp-image-lower-layer img shadow-layer)
   (gimp-image-lower-layer img shadow-layer)
   (gimp-image-resize-to-layers img)
))
;
; End Subroutine Apply

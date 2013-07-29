;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Lava Logo V1.2 (11-2007) script  for GIMP 2.4
; Created by Scott Mosteller - 10-2007
; Comments directed to http://www.upstateforums.com (computer graphics and art section)
;
; Tags: logo, lava
;
; Author statement:
;
; Interesting text effects can be achieved by varying font, text-color, shadow-color, gradient and lava roughness
;
; Unchecking the animation option produces a static image with one text layer with shadow (semi-flattened)
; Checking the animation option produces 4 layers, drop shadowed and semi-flattened for use as an animated gif (just save as gif)
;
; Checking raw layers options dispays raw layers only
;
; Compatible with all Gimp Versions 2.x - Enjoy!
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;
; V1.0 - Initial Release
; V1.1 - Corrected Bug With Alpha-to-logo
;        Added Shadow Offset Sialog
; V1.2 - Correct Bug With Text Color
;        Added Animation and Raw layers options
;        Revamp Code Structure
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

; Create lava text with drop shadow
;
; User Options Popup
;
(script-fu-register "script-fu-lava-logo"
            _"_Lava Logo..."
            "Creates a lava logo with a drop shadow"
            "Scott Mosteller"
            "Scott Mosteller"
            "2007"
            ""
            SF-STRING     _"Text"               "UFSC"
            SF-ADJUSTMENT _"Font size (pixels)" '(200 2 1000 1 10 0 1)
            SF-FONT       _"Font"               "Arial Bold"
            SF-COLOR      _"Background color"   '(255 255 255)
            SF-COLOR      _"Text color"         '(0 0 0)
            SF-COLOR      _"Shadow color"       '(0 0 0)
                    SF-ADJUSTMENT _"Shadow Offset X"    '(4 -99 99 1 1 0 1)
                    SF-ADJUSTMENT _"Shadow Offset Y"    '(4 -99 99 1 1 0 1)
                    SF-ADJUSTMENT _"Shadow Opacity"     '(60 1 100 1 1 0 1)
                    SF-GRADIENT   _"Lava Gradient"      "German flag smooth"
                    SF-TOGGLE     _"Use Active Gradient?"  FALSE
                    SF-ADJUSTMENT _"Lava Roughness"    '(7 2 20 1 1 0 1)
                    SF-TOGGLE     _"Animate?"           TRUE
                    SF-TOGGLE     _"Raw Layers Only?"   FALSE)
;
; Register on Menu
;
(script-fu-menu-register "script-fu-lava-logo"
             _"<Toolbox>/Xtns/FX-Foundry/Logos")
;
; Define Main Lava-Logo Function
;

(define (script-fu-lava-logo text
                   size
                   font
                   bg-color
                   text-color
                               shadow-color
                               shx
                               shy
                               sho
                               lgrad
                               active
                               lruff
                               lani
                               raw)
  (let* ((img (car (gimp-image-new 256 256 RGB)))
    (tmpfg (car (gimp-context-get-foreground img)))
    (tmpbg (car (gimp-context-get-background img)))
    (tmp (car (gimp-context-set-foreground text-color)))
    (text-layer (car (gimp-text-fontname img -1 0 0 text 10 TRUE size PIXELS font)))
    (text-layer2 (car (gimp-text-fontname img -1 0 0 text 10 TRUE size PIXELS font)))
    (text-layer3 (car (gimp-text-fontname img -1 0 0 text 10 TRUE size PIXELS font)))
    (text-layer4 (car (gimp-text-fontname img -1 0 0 text 10 TRUE size PIXELS font)))
    (lay1 0)
    (lay2 0)
    (lay3 0)
    (lay4 0))
;
; Lava Text Main Procedure Body
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
    (gimp-image-undo-disable img)
    (gimp-drawable-set-name text-layer text)
    (apply-lava-logo-effect img text-layer bg-color text-color shadow-color shx shy sho lgrad active lruff lani raw tmpfg tmpbg)
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
    (gimp-drawable-set-visible text-layer2 1)
    (apply-lava-logo-effect img text-layer2 bg-color text-color shadow-color shx shy sho lgrad active lruff lani raw tmpfg tmpbg)
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
    (gimp-drawable-set-visible text-layer3 1)
    (apply-lava-logo-effect img text-layer3 bg-color text-color shadow-color shx shy sho lgrad active lruff lani raw tmpfg tmpbg)
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
    (gimp-drawable-set-visible text-layer4 1)
    (apply-lava-logo-effect img text-layer4 bg-color text-color shadow-color shx shy sho lgrad active lruff lani raw tmpfg tmpbg)
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
        ))
 ))

(if (= lani FALSE)
          (begin
           (gimp-image-remove-layer img text-layer2)
           (gimp-image-remove-layer img text-layer3)
           (gimp-image-remove-layer img text-layer4)
))

    (gimp-selection-none img)
    (gimp-image-undo-enable img)
    (gimp-display-new img)))
;
; End Lava Text Main Procedure
;
;
; Define Lava Logo Apply Routine
;
;
(define (apply-lava-logo-effect img
                  logo-layer
                  bg-color
                  text-color
                                  shadow-color
                                  shx
                                  shy
                                  sho
                                  lgrad
                                  active
                                  lruff
                                  lani
                                  raw
                                  tmpfg
                                  tmpbg)
  (let* ((width (car (gimp-drawable-width logo-layer)))
     (height (car (gimp-drawable-height logo-layer)))
     (shadow-layer (car (gimp-layer-new img width height RGBA-IMAGE "Shadow" sho NORMAL-MODE))))
;
; Subroutine - Lava Logo Apply Body
;

   (gimp-selection-none img)
   (script-fu-util-image-resize-from-layer img logo-layer)

;
; Set color for active gradient & call Lava effects procedure
;
   (gimp-context-set-foreground tmpfg)
   (gimp-context-set-background tmpbg)
   (script-fu-lava img logo-layer 10 10 lruff lgrad 1 1 active)

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
   (gimp-image-lower-layer img shadow-layer)
   (gimp-image-resize-to-layers img)
))
;
; End Subroutine Apply
;

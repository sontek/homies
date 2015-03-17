; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Liquid Water script for GIMP 2.4
; Copyright (C) 2001 Laetitia Marin titix@gimpforce.org
; Copyright (C) 2001 Ostertag Raymond coordinateur@gimp-fr.org
; Copyright (C) 2007 Philippe Demartin philippe@demartinenchile.com Paint corroded version for GIMP 2.4
;
; Tags: logo
;
; Author statement:
;
; This is the official English version you'll find a french version at http://www.gimp-fr.org/
; Script-fu Liquid-Water  an attempt to realise some funy water effect
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
; version 0.2 2007-october-21
;     - Initial relase
; --------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
; --------------------------------------------------------------------

(define (Aply-script-fu-Liquid-Water img Bump-Layer Back-color ShapeW DropsA LightA )
  (let* ((width (car (gimp-drawable-width Bump-Layer)))
    (height (car (gimp-drawable-height Bump-Layer)))
        (Noise_width 0)
        (Noise_height 0)
        (DropsA (*  DropsA 2.55))
        (ShapeW (/  ShapeW 2))
        (theLayers 0)
        (theLayersArray 0)
        (DropShadow 0)
        (activ_selection (car (gimp-selection-is-empty img)))
    (Noise_calque (car (gimp-layer-new img width height RGBA-IMAGE "Noise" 100 3)))
    (Background-Color_calque (car (gimp-layer-new img width height RGBA-IMAGE "Background-Color" 100
                                                                                 NORMAL)))
    (Mosaic_calque (car (gimp-layer-new img width height RGBA-IMAGE "Mosaic" 100 NORMAL)))
    (Water_calque (car (gimp-layer-new img width height RGBA-IMAGE "Water" 100 NORMAL)))
    (old-fg (car (gimp-palette-get-foreground)))
    (old-bg (car (gimp-palette-get-background)))
        (White '(255 255 255))
        (Black '(0 0 0))
        (Grey_1 '(43 43 43))
        (Grey_2 '(211 211 211)))

    ; undo initialisation
    (gimp-image-undo-group-start img)

    (gimp-image-resize-to-layers img)
(let* ((height (car (gimp-image-height img)))
       (width  (car (gimp-image-width  img))))
       (gimp-selection-all img)

    ; Create Layer and fill them
    (gimp-image-add-layer img Background-Color_calque 0)
        (gimp-context-set-foreground Back-color)
        (gimp-bucket-fill Background-Color_calque 0 0 100 0 FALSE 0 0)
    (gimp-image-add-layer img Mosaic_calque 0)
        (gimp-context-set-foreground Grey_2)
        (gimp-bucket-fill Mosaic_calque 0 0 100 0 FALSE 0 0)
    (gimp-image-add-layer img Noise_calque 0)
        (gimp-context-set-foreground White)
        (gimp-bucket-fill Noise_calque 0 0 100 0 FALSE 0 0)

    ; Create Noise
    ;(gimp-image-set-active-layer img Noise_calque)
       (set! Noise_height (/ height 10))
       (set! Noise_width  (/ width  10))
    (gimp-layer-scale Noise_calque Noise_width Noise_height 0)
    (plug-in-rgb-noise 1 img Noise_calque FALSE FALSE 1 1 1 0)
    (gimp-layer-scale Noise_calque width height 0)
    (gimp-layer-resize-to-image-size Noise_calque)

    ; Transforming the text
    (gimp-image-raise-layer-to-top img Bump-Layer)
    (gimp-selection-layer-alpha Bump-Layer)
    (gimp-context-set-foreground Black)
    (gimp-bucket-fill Bump-Layer 0 0 100 0 FALSE 0 0)
    (gimp-selection-invert img)
    (gimp-context-set-foreground White)
    (gimp-bucket-fill Bump-Layer 0 0 100 0 FALSE 0 0)
    (gimp-selection-all img)
    (plug-in-gauss 1 img Noise_calque 20 20 0)
    (gimp-threshold Noise_calque DropsA 255)    ;
    (plug-in-gauss 1 img Bump-Layer  ShapeW ShapeW 0)
    (gimp-threshold Bump-Layer 195 255)
    (gimp-image-raise-layer-to-top img Noise_calque)
    (gimp-image-merge-down img Noise_calque 0)
    (set! theLayers (gimp-image-get-layers img))
    (set! theLayersArray (cadr theLayers))
    (set! Bump-Layer (aref theLayersArray 0))

   ;Create floor texure
    (gimp-context-set-background Black)
    (gimp-context-set-foreground Black)
    (plug-in-mosaic 1 img Mosaic_calque 80 6 1 1 TRUE 135 0.1 TRUE FALSE 0 0 1)

     ; preparing the layers for bumping
(let* ((Highlight_up   (car (gimp-layer-copy Bump-Layer FALSE)))
      (Highlight_down  (car (gimp-layer-copy Bump-Layer FALSE)))
      (Highlight_fill  (car (gimp-layer-copy Bump-Layer FALSE)))
      (Shadow  (car (gimp-layer-copy Bump-Layer FALSE))))
      (gimp-image-add-layer img   Highlight_up 0)
      (gimp-image-add-layer img Highlight_down 0)
      (gimp-image-add-layer img Highlight_fill 0)
      (gimp-image-add-layer img Shadow 4)

    (gimp-selection-none img)
    (gimp-by-color-select   Bump-Layer '(255 255 255) 0 0 FALSE FALSE 0 FALSE)
    (gimp-edit-clear   Highlight_up)
    (gimp-edit-clear Highlight_down)
    (gimp-edit-clear Highlight_fill)
    (gimp-edit-clear       Shadow)
    (gimp-selection-invert img)
    (gimp-context-set-foreground Grey_1)
    (gimp-bucket-fill Highlight_up 0 0 100 0 FALSE 0 0)
    (gimp-bucket-fill Highlight_down 0 0 100 0 FALSE 0 0)
    (gimp-bucket-fill Highlight_fill 0 0 100 0 FALSE 0 0)
    (gimp-selection-all img)
    (plug-in-gauss 1 img Bump-Layer 20 20 0)
    (plug-in-gauss 1 img Shadow 10 10 0)
    (gimp-layer-set-offsets Shadow 4 3)

    ;Bumping an Highlight
    (plug-in-bump-map TRUE img Highlight_up Bump-Layer    130 15 30 0 0 0 0 TRUE TRUE 3)
    (plug-in-bump-map TRUE img Highlight_down Bump-Layer  300 30 30 0 0 0 0 TRUE TRUE 3)
    (gimp-selection-layer-alpha Highlight_fill)
    (gimp-edit-clear Shadow)
    (plug-in-displace 1 img Mosaic_calque 2 -4 2 2 Bump-Layer Bump-Layer 0)
    (plug-in-lighting
                       1 img Highlight_up Highlight_up  Highlight_up FALSE FALSE FALSE 0 White
                       -1 -1 1 -4 -0 1 LightA
                       1 0.6 2 40 TRUE FALSE FALSE)
    (gimp-selection-none img)
    (gimp-by-color-select   Highlight_up Grey_1 30 0 FALSE TRUE 10 FALSE)
    (gimp-edit-clear  Highlight_up)
        (gimp-selection-none img)
    (gimp-selection-none img)
    (gimp-by-color-select Highlight_down Grey_1 10 0 FALSE TRUE 5 FALSE)
    (gimp-edit-clear  Highlight_down)
        (gimp-selection-none img)
    (plug-in-lighting
                       1 img Mosaic_calque Mosaic_calque Mosaic_calque  1 FALSE FALSE 0 White
                       -1 -1 1 -4 -0 1 LightA
                       0.2 0.6 0.5 27 TRUE FALSE FALSE)
    (gimp-selection-layer-alpha Highlight_fill)
    (plug-in-bump-map TRUE img Mosaic_calque Bump-Layer  135 13.5 1 10 10 0 0 TRUE TRUE 3)

    ;Layer seting adjustment
    (gimp-selection-all img)
    (gimp-layer-set-opacity Mosaic_calque 70)
    (gimp-layer-set-opacity Shadow 30)
    (gimp-layer-set-opacity Highlight_up 80)
    (gimp-layer-set-opacity Highlight_down 85)
    (gimp-layer-set-opacity Highlight_fill 15)
    (gimp-image-lower-layer img Highlight_fill)
    (gimp-image-lower-layer img Highlight_fill)
    (gimp-layer-set-mode Highlight_up 4)
    (gimp-layer-set-mode Highlight_down 4)
    (gimp-layer-set-mode Mosaic_calque 18)
    (gimp-layer-set-visible Bump-Layer FALSE)
    (gimp-layer-resize-to-image-size Shadow)
    (gimp-layer-set-name Shadow "Shadow")
    (gimp-layer-set-name Highlight_up "Highlight_up")
    (gimp-layer-set-name Highlight_down "Highlight_down")
    (gimp-layer-set-name Highlight_fill "Highlight_fill")
    (gimp-layer-set-name Bump-Layer "Bump-Layer")

    (gimp-palette-set-foreground old-fg)
    (gimp-palette-set-background old-bg)
    ;Finish the undo group for the process
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
))))

(define (script-fu-Liquid-Water-logo-alpha img Bump-Layer Back-color  ShapeW DropsA LightA)
(begin
         (Aply-script-fu-Liquid-Water img Bump-Layer Back-color ShapeW DropsA LightA)
         (gimp-displays-flush)))

(script-fu-register "script-fu-Liquid-Water-logo-alpha"
            _"<Image>/FX-Foundry/Alpha to Logo/Liquid-Water"
            "Liquid-Water effect"
            "www.demartinenchile.com"
            "2007 Philippe Demartin"
            "20.10.2007"
            ""
                   SF-IMAGE "Image" 0
                SF-DRAWABLE "Drawable" 0
            SF-COLOR "Background" '(0 0 255)
                SF-ADJUSTMENT "Shape watering" '(20 0 100 1 1 2 0)
                SF-ADJUSTMENT "Water drops Amount" '(60 1 100 1 1 2 0)
                SF-ADJUSTMENT "Light Amount" '(0.80 0 1.6 0.01 1 2 0))

(define (script-fu-Liquid-Water-logo font size text Back-color ShapeW DropsA LightA )

  (let* ((img (car (gimp-image-new 256 256  RGB)))    ; nouvelle image -> img
    ; (border (/ size 4))
         (text-layer (car (gimp-text-fontname img -1 0 0 text size TRUE size PIXELS font)))

     )

    (gimp-layer-new img 256 256 RGBA-IMAGE "background" 90 0)
    (gimp-image-undo-disable img)
    (Aply-script-fu-Liquid-Water img text-layer Back-color ShapeW DropsA LightA )
    (gimp-image-undo-enable img)
    (gimp-display-new img)
    ))

(script-fu-register     "script-fu-Liquid-Water-logo"
            "Liquid Water"
            "Create a Water logo with random drops"
            "Philippe Demartin"
            "www.demartinenchile.com"
            "10/21/2007"
            ""
            SF-FONT "Font Name" "Tahoma Bold"
            SF-ADJUSTMENT "Font size (pixels)" '(120 50 1000 1 10 0 1)
            SF-STRING "Enter your text" "Liquid Water..."
            SF-COLOR "Background" '(0 0 255)
                SF-ADJUSTMENT "Shape watering" '(40 2 100 1 1 2 0)
                SF-ADJUSTMENT "Water drops Amount" '(60 0 100 1 1 2 0)
                SF-ADJUSTMENT "Light Amount" '(0.80 0 1.6 0.01 1 2 0)
            )

(script-fu-menu-register "script-fu-Liquid-Water-logo"
             "<Toolbox>/Xtns/Logos")

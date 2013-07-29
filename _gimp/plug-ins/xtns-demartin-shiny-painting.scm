;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Shiny painting script  for GIMP 2.4
; Copyright (C) 2001 Laetitia Marin titix@gimpforce.org
; Copyright (C) 2001 Ostertag Raymond coordinateur@gimp-fr.org
; Copyright (C) 2007 Philippe Demartin philippe@demartinenchile.com Paint corroded version for GIMP 2.4
;
; Tags: logo
;
; Author statement:
;
; This is the official English version you'll find a french version at http://www.gimp-fr.org/
;
; Script-fu Shiny corroded Painting an attempt to realise the Scott-Effect with painted surface
;
; Start : a selection in an image or a layer with a transparency area who will be transformed
; in selection
;
; See the manual at the tutorial section of the gug http://gug.sunsite.dk/
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; version 0.2 2007-october-21
;     - Initial relase
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
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (Aply-script-fu-shiny img Bump-Layer fond-color damage LightA LightPX LightPY Bcolor Crackeled bumpmap_depth)
  (let* ((sizeX (car (gimp-drawable-width Bump-Layer)))
    (sizeY (car (gimp-drawable-height Bump-Layer)))
        (Bunped_layer (car (gimp-layer-copy Bump-Layer FALSE)))
        (seed (* 30 30))
        (activ_selection (car (gimp-selection-is-empty img)))
        (damage (* damage 2.55))
    (calque1 (car (gimp-layer-new img sizeX sizeY RGBA-IMAGE "plasma" 100 NORMAL)))
        (masque1 (car (gimp-layer-create-mask calque1 1)))
    (old-fg (car (gimp-palette-get-foreground)))
    (old-bg (car (gimp-palette-get-background)))
        (blanc '(255 255 255))
        (pick_color '(255 255 255))

        )

    ; undo initialisation
    (gimp-image-undo-group-start img)

    (gimp-image-resize-to-layers img)
    (gimp-selection-layer-alpha Bump-Layer)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-bucket-fill Bump-Layer 0 0 100 0 FALSE 0 0)
    (gimp-selection-invert img)
    (gimp-context-set-foreground fond-color)
    (gimp-bucket-fill Bump-Layer 0 0 100 0 FALSE 0 0)
    (gimp-image-add-layer img Bunped_layer 0)
    (gimp-selection-invert img)

    ; layer 1
    (gimp-image-add-layer img calque1 0)
    (gimp-image-add-layer-mask img calque1 masque1)

    ; plasma
    (set! seed (* 30 30))
    (plug-in-plasma TRUE img calque1 TRUE 2.5)
    (gimp-threshold calque1 damage 255)
    (let* ((calque2 (car (gimp-layer-copy calque1 TRUE)))
       (masque2 (car (gimp-layer-mask calque2))))


    ; layer 2
      (gimp-image-add-layer img calque2 0)

    ; fill the layer-mask
      (gimp-palette-set-foreground blanc)
      (set! activ_selection (car (gimp-selection-is-empty img)))
      (cond
       ((= activ_selection 0) ; selection activ

    (gimp-bucket-fill masque1 0 0 100 0 FALSE 0 0)
    (gimp-bucket-fill masque2 0 0 100 0 FALSE 0 0)
    (gimp-selection-none img))
       ((= activ_selection 1) ; no selection activ
    (gimp-selection-layer-alpha Bump-Layer)
    (gimp-bucket-fill masque1 0 0 100 0 FALSE 0 0)
    (gimp-bucket-fill masque2 0 0 100 0 FALSE 0 0)
    )
       ) ; end of cond





(gimp-by-color-select calque1 '(255 255 255) 0 0 FALSE FALSE 0 FALSE)
(gimp-context-set-background Bcolor)
(gimp-bucket-fill calque2 1 0 100 0 FALSE 0 0)
(gimp-selection-invert img)
(gimp-edit-clear calque2)
(gimp-selection-all img)

    ; Bumping the letters
      (plug-in-gauss 1 img Bump-Layer 10 10 0)
      (plug-in-bump-map TRUE img Bunped_layer Bump-Layer  125 30 bumpmap_depth 0 0 0 0 TRUE TRUE LINEAR)
    ; bumpmap on layer 2
      (plug-in-bump-map TRUE img calque2 Bump-Layer     125 30 bumpmap_depth 0 0 0 0 TRUE TRUE LINEAR)
      (plug-in-bump-map TRUE img calque2 calque2     125 45 bumpmap_depth 0 0 0 0 TRUE FALSE LINEAR)

    ;Light efect
      (plug-in-lighting 1 img calque2 calque2 0 TRUE FALSE 0 0 blanc LightPX LightPY                               LightA -1.19 -7.14 1.00 0.9 2 2 LightA 10 TRUE FALSE FALSE)
    (gimp-layer-set-mode calque1 8)
    (gimp-layer-set-offsets Bump-Layer 18 12)
    (plug-in-gauss 1 img Bump-Layer 20 20 0)
    (gimp-layer-resize-to-image-size Bump-Layer)
    (gimp-selection-layer-alpha Bump-Layer)
    (gimp-selection-invert img)
    (gimp-context-set-foreground fond-color)
    (gimp-bucket-fill Bump-Layer 0 0 100 0 FALSE 0 0)
    (gimp-selection-all img)
    ; back to the initials colours and display the result

    (gimp-palette-set-foreground old-fg)
    (gimp-palette-set-background old-bg)

    (if (= Crackeled TRUE)
    ; layer 3
    (let* ((calque3 (car (gimp-layer-copy calque2 TRUE))))
           (gimp-image-add-layer img calque3 0)
    (gimp-selection-layer-alpha calque2)
    (gimp-palette-set-background Bcolor)
    (plug-in-mosaic 1 img calque2 10 10 1 0 TRUE 175 0.3 TRUE FALSE 3 0 1)
    (gimp-layer-set-mode calque3 21)))
    (gimp-selection-all img)

    (gimp-palette-set-foreground old-fg)
    (gimp-palette-set-background old-bg)
    ;Finish the undo group for the process
    (gimp-image-undo-group-end img)

      (gimp-displays-flush))))

(define (script-fu-shiny-logo-alpha img Bump-Layer fond-color damage LightA LightPX LightPY Bcolor Crackeled bumpmap_depth)
(begin
(Aply-script-fu-shiny img Bump-Layer fond-color damage LightA LightPX LightPY Bcolor Crackeled bumpmap_depth)
 (gimp-displays-flush)))

(script-fu-register "script-fu-shiny-logo-alpha"
            _"<Image>/FX-Foundry/Alpha to Logo/Corroded Painting"
            "Scott-effect : Shiny Corroded Painting"
            "titix raymond and philippe"
            "2001, titix and raymond 2007 Philippe Demartin"
            "20.10.2007"
            ""
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Drawable" 0
                    SF-COLOR "Background" '(178 178 178)
                    SF-ADJUSTMENT "Painting Damage %" '(70 10 100 1 0 0 0)
            SF-ADJUSTMENT "Light Amount" '(0.70 0 10 0.1 1 2 0)
            SF-ADJUSTMENT "Light Position X" '(0 -50 50 1 0 0 0)
            SF-ADJUSTMENT "Light Position y" '(0 -50 50 1 0 0 0)
            SF-COLOR "Painting Color" '(255 0 0)
            SF-TOGGLE "Crackeled" FALSE
                    SF-ADJUSTMENT "Bumpmap depth" '(15 1 50 1 0 0 0))

(define (script-fu-shiny-logo font text Text-Color Back-color size damage LightA LightPX LightPY Bcolor Crackeled bumpmap_depth)

  (let* ((img (car (gimp-image-new 256 256  RGB)))    ; nouvelle image -> img
     (border (/ size 4))
    ; (background (car (gimp-layer-new img 256 256 RGBA-IMAGE "background" 90 0)))
         (Text-Color (car  (gimp-context-set-foreground Text-Color)))
         ; (Back-color (car  (gimp-context-set-background Back-color)))
     (text-layer (car (gimp-text-fontname img -1 0 0 text border TRUE size PIXELS font)))

     )

    (gimp-layer-new img 256 256 RGBA-IMAGE "background" 90 0)
    ;(gimp-edit-bucket-fill-full background 1 0 100 255 FALSE FALSE 0 0 0 )
    (gimp-image-undo-disable img)
    ;(gimp-drawable-set-name text-layer text)
    (Aply-script-fu-shiny img text-layer Back-color damage LightA LightPX LightPY Bcolor Crackeled bumpmap_depth)
    (gimp-image-undo-enable img)
    (gimp-display-new img)
    ))

(script-fu-register     "script-fu-shiny-logo"
            "Corroded Painting"
            "Create corroded painted logo"
            "Philippe Demartin"
            "Inspired from the Corrosion script from titix and raymond"
            "10/21/2007"
            ""
            SF-FONT "Font Name" "Tahoma Bold"
            SF-STRING "Enter your text" "Corroded..."
            SF-COLOR "Font Color" '(133 52 2)
            SF-COLOR "Background" '(178 178 178)
            SF-ADJUSTMENT "Font size (pixels)" '(150 2 1000 1 10 0 1)
                    SF-ADJUSTMENT "Painting Damage %" '(70 10 100 1 0 0 0)
            SF-ADJUSTMENT "Light Amount" '(0.70 0 10 0.01 1 2 0)
            SF-ADJUSTMENT "Light Position X" '(0 -2 2 0.1 1 1 0)
            SF-ADJUSTMENT "Light Position y" '(0 -2 2 0.1 1 1 1)
            SF-COLOR "Painting Color" '(255 0 0)
            SF-TOGGLE _"Crackeled" FALSE
                    SF-ADJUSTMENT "Bumpmap depth" '(15 1 50 1 0 0 0)            )

(script-fu-menu-register "script-fu-shiny-logo"
             "<Toolbox>/Xtns/FX-Foundry/Logos")

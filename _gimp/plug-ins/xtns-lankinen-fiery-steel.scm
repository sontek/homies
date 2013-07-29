;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Fiery Steel script  for GIMP 2.4
; Copyright (C) 9.5.1998 Urpo Lankinen.
; E-mail: <wwwwolf@iki.fi> Homepage: <URL:http://www.iki.fi/wwwwolf/>
;
; Tags: logo
;
; Author statement:
;  Distributed under GPL. Permission granted to distribute this script
;  with *anything* that has *something* to do with The GIMP.
;
; This script was inspired by "Terminator 2: Judgement Day"
; opening scene.
;
; The result: letters of steel (well, aluminium...) in hellfire.
; "You know, it was not really easy for a mere layman to figure out
; the flame-generation script"... but with a dragon around, it was
; much easier.
;
; For coolest results, Use Scott font from CorelDRAW!...
; (But Crillee/FreeFonts produces pretty kewl results, too.)
;
; I should make that Slab thing to make better "Terminator 2:
; Lookandfeeliation Day" effect...
; Terminator 2 fonts for free, anyone?
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;
; Greatly enhanced on 19.5.1998.
; Also added some tiny little enhancements on 29.5.1998
; More enhancements 18-19.7.1998
; Updated to Gimp2.4 (11-2007) http://www.gimpscripts.com
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


;
;
;
(define (tmp-layer))
(define (bump-layer))
(define (fire1-layer))
(define (fire-layer))
(define (fire1-layer))
(define (fire2-layer))
(define (fire3-layer))
(define (turbul-layer))
(define (turbul2-layer))
(define (sendtog))
(define (script-fu-fiery-steel
     text font size grad perc txtbord darkentog
     ; slabtog
     fierytog sprd rndamt fulltog
     turbultog turbulence
;  turbdetail
;     sendtog ; Does not work, drats...
     )
  ; �ydx, that was a looooong feature list.
  (let* ((img (car (gimp-image-new 256 256 RGB)))
     (bg-layer (car (gimp-text-fontname img -1 0 0 text 10 TRUE
                        size PIXELS font)))
     (text-layer (car (gimp-text-fontname img -1 0 0 text 10 TRUE
                        size PIXELS font)))
     (width (car (gimp-drawable-width text-layer)))
     (height (car (gimp-drawable-height text-layer)))
     (text2-layer (car (gimp-text-fontname img -1 0 0 text 10 TRUE
                        size PIXELS font)))
     (brd-layer
      (car (gimp-layer-new img width height
                   GRAYA-IMAGE "border" 100 OVERLAY)))

     (old-fg (car (gimp-palette-get-foreground)))
     (old-bg (car (gimp-palette-get-background))))

    ; Previously it just jammed at merge-visible-layers, but when
    ; I added this, it somehow started working... Dunno why. =/
    (verbose 4)

    (gimp-image-undo-disable img)
    (gimp-layer-set-name text-layer "Text")
    (gimp-layer-set-name text2-layer "Text2")
    (gimp-layer-set-name bg-layer "Bg")
    (gimp-image-resize img width height 0 0)

    ; �KS�N =========================================================

    ; Background

    (gimp-image-set-active-layer img bg-layer)
    (gimp-selection-all img)
    (gimp-palette-set-foreground '(0 0 0))
    (gimp-bucket-fill bg-layer FG-BUCKET-FILL NORMAL 100 255 FALSE 1 1)
    (gimp-palette-set-foreground old-fg)
    (gimp-selection-none img)

    ; Do the gradient
    (gimp-image-set-active-layer img text2-layer)
    (gimp-layer-set-preserve-trans text2-layer TRUE)     ; Preserve trans.
    (gimp-context-set-gradient grad)
    (gimp-blend text2-layer
        CUSTOM NORMAL LINEAR 100 0 REPEAT-NONE FALSE FALSE 0 3 0.2
        (/ width 2) 0 (/ width 2) height)
    (gimp-layer-set-preserve-trans text2-layer FALSE)
    ; and hide the bg and this new one
    (gimp-layer-set-visible bg-layer FALSE)
    (gimp-layer-set-visible text-layer TRUE)
    (gimp-layer-set-visible text2-layer FALSE)

    ; Add white layer
    (set! tmp-layer
      (car (gimp-layer-new img width height RGB-IMAGE "Temp" 100 NORMAL)))
    (gimp-image-add-layer img tmp-layer 3)
    (gimp-image-set-active-layer img tmp-layer)
    (gimp-selection-all img)
    (gimp-palette-set-foreground '(255 255 255))
    (gimp-bucket-fill tmp-layer FG-BUCKET-FILL NORMAL 100 255 FALSE 1 1)
    (gimp-palette-set-foreground old-fg)
    (gimp-selection-none img)

    ; Merge, blur, bump
    (set! bump-layer
      (car (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY)))
    (gimp-layer-set-visible bg-layer TRUE)
    (plug-in-gauss-iir 1 img bump-layer txtbord TRUE TRUE)
    (gimp-layer-set-visible bump-layer FALSE)
    (gimp-layer-set-visible text2-layer TRUE)

    ; Twiddle the brightness a little
    (if (eq? darkentog TRUE)
    (gimp-brightness-contrast text2-layer -20 -20))

    ; Bumpmapping depends on do we need the flames or not...
    (plug-in-bump-map 1 img text2-layer bump-layer
              (if (eq? fierytog FALSE) 135.00 90.00)
              (if (eq? fierytog FALSE) 45.00 30.0)
              6 0 0 0 0 FALSE FALSE LINEAR)

    (gimp-image-remove-layer img bump-layer)

    ; And that was the easy part. ::sigh::

    (if (eq? fierytog TRUE)
    (let* ((sheight (* height perc))
          (ycoord (- height sheight)))
                    ; Add a layer
      (set! fire1-layer
        (car (gimp-layer-new img width height RGBA-IMAGE "Fire 1"
                     100 NORMAL)))
      (gimp-image-add-layer img fire1-layer 0)
                    ; Clear it
      (gimp-selection-all img)
      (gimp-edit-clear fire1-layer)
                    ; Make lower part red
;      (gimp-rect-select img 0 ycoord width sheight REPLACE FALSE 0)
      (gimp-palette-set-foreground '(180 0 20))
      (gimp-bucket-fill fire1-layer FG-BUCKET-FILL NORMAL
                100 255 FALSE 1 1)
      (gimp-selection-none img)

      ; Do nasty stuff to the lower fire layer
      (plug-in-ripple 1 img fire1-layer
              (/ width 8) (/ height 8) 1 0 1 FALSE FALSE)
      (plug-in-whirl-pinch 1 img fire1-layer
                   45 0 0.7)

      ; Copy the layer, make it yellow, shift down a bit
      (set! fire2-layer
        (car (gimp-layer-copy fire1-layer TRUE)))
      (gimp-image-add-layer img fire2-layer 0) ; top
      (gimp-layer-set-name fire2-layer "Fire 2")

      (gimp-image-set-active-layer img fire2-layer)
      (gimp-layer-set-preserve-trans fire2-layer TRUE)
      (gimp-palette-set-foreground '(228 170 4)) ; Yellow
      (gimp-selection-all img)
      (gimp-bucket-fill fire2-layer FG-BUCKET-FILL NORMAL
                100 255 FALSE 1 1)
      (gimp-layer-set-preserve-trans fire2-layer FALSE)
      (gimp-selection-none img)

      (gimp-channel-ops-offset fire2-layer FALSE 1 0 (/ height 8))

      ; The third fire layer
      (set! fire3-layer
        (car (gimp-layer-copy fire1-layer TRUE)))
      (gimp-image-add-layer img fire3-layer 0) ; top
      (gimp-layer-set-name fire3-layer "Fire 3")
      (gimp-channel-ops-offset fire3-layer FALSE 1 0
                   (* 2.6 (/ height 8)))

      ; resize the said layer...
      (gimp-image-set-active-layer img fire3-layer)
      (gimp-selection-all img)
      (gimp-scale fire3-layer TRUE 0
              (* height 0.6) width height)

      ; spread, spindle, mutilate
      (plug-in-spread 1 img fire1-layer 0 (* 3 sprd))
      (plug-in-spread 1 img fire2-layer 0 (* 2 sprd))
      (plug-in-spread 1 img fire3-layer 0 (* 1.5 sprd))

      ; Merge the layers
      (gimp-layer-set-visible text2-layer FALSE)
      (gimp-layer-set-visible bg-layer FALSE)

      (set! fire-layer
        (car (gimp-image-merge-visible-layers
              img EXPAND-AS-NECESSARY)))
      (gimp-layer-set-name fire-layer "Fire")

      (gimp-layer-set-preserve-trans fire-layer TRUE)
      (plug-in-gauss-rle 1 img fire-layer (* 2 sprd) TRUE TRUE)
      (gimp-layer-set-preserve-trans fire-layer FALSE)

      (gimp-layer-set-visible text2-layer TRUE)
      (gimp-layer-set-visible bg-layer TRUE)

      (plug-in-noisify 1 img fire-layer TRUE 0.2 0.2 0.2 0.2)
      (plug-in-noisify 1 img fire-layer TRUE 0.2 0.2 0.2 0.2)

      (plug-in-gauss-rle 1 img fire-layer (* 2 sprd) TRUE TRUE)

      ; Then, Let's nastyize it.
      (gimp-layer-set-preserve-trans fire-layer TRUE)
      (plug-in-randomize-pick 1 img fire-layer rndamt 80 10 6942)
      (gimp-layer-set-preserve-trans fire-layer FALSE)

      ; Here we use another kewl displacement thing... it really rules.
      ; Displacement was one of the coolest things I knew about back
      ; in 0.59 era.

      (if (eq? turbultog TRUE)
          (begin
        (set! turbul-layer
              (car (gimp-layer-new img width height
                       RGBA-IMAGE "Turbul"
                       100 NORMAL)))
        (gimp-image-add-layer img turbul-layer 0)


        ; We used Solid Noise here, but it didn't look very cool.
        ; This actually works.
        (plug-in-plasma 1 img turbul-layer 42 turbulence)
        (gimp-desaturate turbul-layer)
        (plug-in-c-astretch 1 img turbul-layer)


        ; Add more turbulence. Heavy magery follows. You are not
        ; expected to understand this. It was a result of all-night
        ; weaving of Spells of Technomancy. (In other words,
        ; dark magic follows, beware!)

        ; New layer
        (set! turbul2-layer
              (car (gimp-layer-new img width height
                       RGBA-IMAGE "Turbul2"
                       100 NORMAL)))
        (gimp-image-add-layer img turbul2-layer 0)
        (gimp-image-set-active-layer img turbul2-layer)
        ; Uh, great. Now more fun.

        ; First half
        (gimp-selection-none img)
        (gimp-rect-select img 0 (/ height 2) width (/ height 2)
                  0 FALSE 10)
        (gimp-palette-set-foreground '(0 0 0)) ; Black
        (gimp-bucket-fill turbul2-layer
                  FG-BUCKET-FILL NORMAL 100 255 FALSE 1 1)

        ; Second half
        (gimp-palette-set-foreground '(255 255 255)) ; White
        (gimp-selection-invert img)
        (gimp-bucket-fill turbul2-layer
                  FG-BUCKET-FILL NORMAL 100 255 FALSE
                  (+ (/ height 2) 1) 1)
        (gimp-selection-none img)

        (plug-in-ripple 1 img turbul2-layer
                (/ width 8) (/ height 4) 1 0 1 FALSE FALSE)

        (plug-in-spread 1 img turbul2-layer (/ width 12) (/ height 12))

        (plug-in-gauss-rle 1 img turbul2-layer (/ width 5) TRUE TRUE)

        ; Bleah
        (plug-in-oilify 1 img turbul2-layer 6 0)

        ; First displacement
        (plug-in-displace 1 img turbul-layer
                  (* 1.5 turbulence) (* 3 turbulence)
                  TRUE TRUE
                  turbul2-layer turbul2-layer 1)
        (gimp-image-remove-layer img turbul2-layer)


        ; Second displacement
        (plug-in-displace 1 img fire-layer
                  (* 1.5 turbulence) (* 3 turbulence)
                  TRUE TRUE
                  turbul-layer turbul-layer 1) ; SMEAR
        (gimp-image-remove-layer img turbul-layer)

        ))

      (plug-in-oilify 1 img fire-layer 3 0)

      ; Finally, make the flames burrrrrn...
      (gimp-layer-set-mode fire-layer ADDITION)

      (if (eq? fulltog FALSE)
          (begin
        (gimp-layer-set-visible bg-layer FALSE)
        (set! text2-layer
              (car (gimp-image-merge-visible-layers
                img EXPAND-AS-NECESSARY)))
        (gimp-layer-set-visible bg-layer TRUE)))
      )) ; End of fiery stuff

    ; End of Action(tm) =============================================
    (gimp-palette-set-background old-bg)
    (gimp-palette-set-foreground old-fg)
    (gimp-image-undo-enable img)
    (gimp-display-new img)))

;
; Hajaa-ho!
;
(script-fu-register
 "script-fu-fiery-steel"
 "<Toolbox>/Xtns/FX-Foundry/Logos/Fiery Steel..."
 "An effect inspired by the Terminator 2: Judgement Day\n opening titles. Metallic letters in hellfire."
 "Weyfour WWWWolf (Urpo Lankinen) <wwwwolf@iki.fi>"
 "Weyfour WWWWolf"
 "9 May 1998 (Enhanced greatly on 19 May, 29 May, 18-19 July)"
 ""
 SF-STRING     "Text String"           "Fiery"
 SF-FONT       "Font"                  "Utopia Bold Italic"
 SF-ADJUSTMENT "Font Size"             '(180 2 1000 1 10 0 1)
 SF-GRADIENT   "Gradient"              "Flare Glow Radial 1"
 SF-ADJUSTMENT "Engulfment percentage" '(1 0 1 0.01 0.1 2 0)
 SF-ADJUSTMENT "Text border"           '(5 0 1000 1 10 0 1)

 SF-TOGGLE     "Darken?"               TRUE

 SF-TOGGLE     "Fire effects?"         TRUE

 SF-ADJUSTMENT "Spread coefficient"    '(35 0 1000 1 10 0 1)
 SF-ADJUSTMENT "# of fire randomizes"  '(20 0 1000 1 10 0 1)

 SF-TOGGLE     "Fire over background"   FALSE

 SF-TOGGLE     "Add flame turbulence"   TRUE
 SF-ADJUSTMENT "Turbulence"             '(2 0 1000 1 10 0 1)
)




; BTW, if you're wondering what "RGRNCA" means, surf to:
; <URL:http://www.iki.fi/wwwwolf/games/nethack/index.html>

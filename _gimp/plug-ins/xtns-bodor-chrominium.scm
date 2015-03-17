;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Chrominium  for GIMP 2.4
; Copyright (C) 2007 Denis Bodor <lefinnois@lefinnois.net>
;
; Tags: logo
;
; Author statement:
; A Script-Fu that create a Chrome effect with gradient shadow
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Updated to Gimp2.4 (11-2007) - http://www.gimpscripts.com
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



(define (apply-chrominium-logo-effect img
                      basetext
                      bgradient
                      gradient
                      color
                      bg-color
                      bsparkles)
    (define (chantext))
    (define (basetextmask))
    (define (shadmask))
    (define (sparkmask))
    (define (set-pt a index x y)
    (prog1
    (aset a (* index 2) x)
    (aset a (+ (* index 2) 1) y)))
    (define (spline-chrome)
      (let* ((a (cons-array 12 'byte)))
        (set-pt a 0 0 0)
        (set-pt a 1 63 254)
        (set-pt a 2 124 59)
        (set-pt a 3 170 255)
        (set-pt a 4 220 125)
        (set-pt a 5 255 255)
        a))
  (let* ((width (car (gimp-drawable-width basetext)))
     (height (car (gimp-drawable-height basetext)))
     (fond (car (gimp-layer-new   img
                      width height RGB-IMAGE
                      "Background" 100 NORMAL-MODE)))
     (mapeux (car (gimp-layer-new img
                      width height RGBA-IMAGE
                      "Mapper" 100 NORMAL-MODE)))
     (shad (car (gimp-layer-new   img
                      width height RGBA-IMAGE
                      "Shadow" 100 NORMAL-MODE)))
     (spark (car (gimp-layer-new  img
                      width height RGBA-IMAGE
                      "Sparkle" 100 NORMAL-MODE)))
     (XXbmapshrink (/ height 75))    ; blur size for bumpmap
     (XXbmapblur (/ height 25))    ; blur size for bumpmap
     (XXshadgrow (/ height 62))    ; blur size for bumpmap
     (XXshadblur (/ height 17))    ; blur size for bumpmap
     (XXsparklen (/ height 12))    ; blur size for bumpmap
     )

    (gimp-context-push)

    (gimp-image-add-layer img shad 2)
    (gimp-edit-clear shad)

    (if(= bsparkles TRUE)
      (begin
    (gimp-image-add-layer img spark 3)
    (gimp-edit-clear spark)
    )
      )

;    (if(= bsparkles FALSE)
;      (begin
;    (set! XXshadgrow (* XXshadgrow 1.5))
;    (set! XXshadblur (* XXshadblur 1.1))
;    )
;      )

    (gimp-image-add-layer img fond 4)
    (gimp-edit-clear fond)
    (gimp-image-add-layer img mapeux 5)
    (gimp-edit-clear mapeux)

    ; filling back with background
    (gimp-context-set-foreground bg-color)
    (gimp-edit-fill fond FOREGROUND-FILL)
    (gimp-selection-none img)

    (script-fu-util-image-resize-from-layer img basetext)

    ; create channel for text
    (gimp-selection-layer-alpha basetext)
    (set! chantext (car (gimp-selection-save img)))
    (set! basetextmask (car (gimp-layer-create-mask basetext ADD-ALPHA-MASK)))
    (gimp-layer-add-mask basetext basetextmask)
    (gimp-selection-all img)
    (gimp-context-set-foreground '(200 200 200))
    (gimp-edit-fill basetext FOREGROUND-FILL)

    ; create bumpmap
    (gimp-selection-none img)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-edit-fill mapeux FOREGROUND-FILL)
    (gimp-selection-load chantext)
    (gimp-selection-shrink img XXbmapshrink) ; 4
    (gimp-context-set-foreground '(255 255 255))
    (gimp-edit-fill mapeux FOREGROUND-FILL)
    (gimp-selection-none img)
    (plug-in-gauss-rle2 1 img mapeux XXbmapblur XXbmapblur) ; 10 10

    ; bumpmapping
    (plug-in-bump-map 1
              img
              basetext
              mapeux
              135.00    ; azimuth
              45.00    ; elevation
              8        ; depth
              0        ; x offset
              0        ; y offset
              0        ; water level
              0        ; ambient
              TRUE    ; compensate darky
              FALSE    ; invert
              0        ; TYPE 0=LINEAR, 1=SPHERICAL, 2=SINUS
              )

    ; curving to gain chrome effect
    (gimp-curves-spline basetext
            0         ; channel to modify 0=VALUE
            12         ; nbr of values (point*2)
            (spline-chrome)    ; array of points
            )

    ; back shadow
    (if (= bgradient TRUE)
      (begin
    (gimp-context-set-gradient gradient)
    (gimp-edit-blend shad
             CUSTOM-MODE
             NORMAL-MODE
             GRADIENT-LINEAR    ; gradient type
             100        ; opacity
             0            ; offset
             REPEAT-NONE    ; repeat
             FALSE        ; reverse
             FALSE        ; supersampling
             0 0        ;
             FALSE        ; dithering
             0 0        ; x1 y1
             width        ; x2
             height        ; y2
             )
    ))
    (if (= bgradient FALSE)
      (begin
    (gimp-context-set-foreground color)
    (gimp-edit-fill shad FOREGROUND-FILL)
      ))
    (gimp-selection-load chantext)
    (gimp-selection-grow img XXshadgrow) ; 5
    (set! shadmask (car (gimp-layer-create-mask shad 4))) ; 4=SELECTION MASK
    (gimp-layer-add-mask shad shadmask)
    (gimp-selection-none img)
    (plug-in-gauss-rle2 1 img shadmask XXshadblur XXshadblur)

    ; back shadow sparkle
    (if(= bsparkles TRUE)
      (begin
    (if (= bgradient TRUE)
      (begin
        (gimp-context-set-gradient gradient)
        (gimp-edit-blend spark
                 CUSTOM-MODE
                 NORMAL-MODE
                 GRADIENT-LINEAR    ; gradient type
                 100        ; opacity
                 0            ; offset
                 REPEAT-NONE    ; repeat
                 FALSE        ; reverse
                 FALSE        ; supersampling
                 0 0        ;
                 FALSE        ; dithering
                 0 0        ; x1 y1
                 width        ; x2
                 height        ; y2
                 )
        ))
    (if (= bgradient FALSE)
      (begin
        (gimp-context-set-foreground color)
        (gimp-edit-fill spark FOREGROUND-FILL)
        ))
    (set! sparkmask (car (gimp-layer-create-mask spark 1))) ; 1=BLACK MASK
    (gimp-layer-add-mask spark sparkmask)
    (gimp-selection-load chantext)
    (gimp-selection-grow img XXshadgrow) ; 5
    (plug-in-randomize-hurl    RUN-NONINTERACTIVE
                img
                sparkmask
                50        ; randomization percentage
                1        ; repeat
                FALSE        ; random seed
                10        ; seed (graine)
                )
    (gimp-selection-none img)
    (gimp-threshold    sparkmask
            253    ; low
            255    ; high
            )

    (plug-in-sparkle    RUN-NONINTERACTIVE
                img
                sparkmask
                0.001        ; lum
                0.50        ; intensity
                XXsparklen    ; spike len ; 20
                4        ; spike nbr
                15        ; angle
                1.00        ; density
                0        ; opacity
                0        ; random hue
                0        ; random saturation
                FALSE        ; preserve lum
                FALSE        ; invert
                FALSE        ; add border
                0        ; color type 0=NATURAL
                )

    (gimp-context-pop))
      )
    )
  )


(define (script-fu-chrominium-logo-alpha img
                      basetext
                      bgradient
                      gradient
                      color
                      bg-color
                      bsparkles)
  (begin
    (gimp-image-undo-group-start img)
    (apply-chrominium-logo-effect img basetext bgradient gradient color bg-color bsparkles)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)))

(script-fu-register "script-fu-chrominium-logo-alpha"
            _"Chrominium..."
            "Chrominium logo"
            "Denis bodor"
            "Denis bodor"
            "2007"
            "RGBA"
                    SF-IMAGE      "Image"                     0
                    SF-DRAWABLE   "Drawable"                  0
            SF-TOGGLE     _"Gradient Fill"       TRUE
            SF-GRADIENT   _"Gradient"            "Full saturation spectrum CCW"
            SF-COLOR      _"Color Fill"        '(0 250 0)
            SF-COLOR      _"Background color"   '(0 0 0)
            SF-TOGGLE      _"Sparkles"        TRUE
            )

(script-fu-menu-register "script-fu-chrominium-logo-alpha"
             _"<Image>/FX-Foundry/Alpha to Logo")


(define (script-fu-chrominium-logo text
                   font
                   size
                   bgradient
                   gradient
                   color
                   bg-color
                   bsparkles)
  (let* ((img (car (gimp-image-new 256 256 RGB)))
     (border (/ size 4))
     (basetext (car (gimp-text-fontname   img
                          -1 0 0 text border TRUE
                          size PIXELS font))))
    (gimp-image-undo-disable img)
    (apply-chrominium-logo-effect img basetext bgradient gradient color bg-color bsparkles)
    (gimp-image-undo-enable img)
    (gimp-display-new img)))

(script-fu-register "script-fu-chrominium-logo"
            _"Chrominium..."
            "Chrominium logo"
            "Denis bodor"
            "Denis bodor"
            "2007"
            ""
            SF-STRING     _"Text"               "Chrome"
            SF-FONT       _"Font"               "Ethnocentric"
            SF-ADJUSTMENT _"Font size (pixels)" '(150 2 1000 1 10 0 1)
            SF-TOGGLE     _"Gradient Fill"       TRUE
            SF-GRADIENT      _"Gradient"         "Full saturation spectrum CCW"
            SF-COLOR      _"Color Fill"        '(0 250 0)
            SF-COLOR      _"Background color"   '(0 0 0)
            SF-TOGGLE      _"Sparkles"        TRUE
                    )

(script-fu-menu-register "script-fu-chrominium-logo"
             _"<Toolbox>/Xtns/FX-Foundry/Logos")

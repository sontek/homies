
;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Photo LAB Wow Kung-fu script  for GIMP 2.4
; Original author: lasm <lasm@rocketmail.com>
;;;  http://www.godsimmediatecontact.com
;;;  http://www.godsdirectcontact.org
;;;  http://www.raindesigninc.com
;
; Tags: color
;
; Author statement:
;
; Welcome to the Line Art Coffee House
; This Photo LAB Wow Kung-fu script is for Kung Fu connisseurs only
; If it doesn't work for your images, perhaps you prefer the Bubble Tea House next door ?
; Lasm's famous Photo LAB script a.k.a. Grand Mother's Kung-fu script
; Dedication - to my mother (1917-2002)
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; version 1.0  by Lasm 2005/05/10 <lasm@rocketmail.com>
;     - Initial release
; version 1.1  by Lasm 2005/13/10 <lasm@rocketmail.com>
;     - Added Shaolin line art and kung-fu
;       kung-fu tweaks the posterize value 2 or 3
; version 1.2  by Lasm 2005/15/10 <lasm@rocketmail.com>
;     - changed to work with Gimp 2.3.4
; version 2.0  by Lasm 2005/16/10 <lasm@rocketmail.com>
;     - Added features to tweak black & white layer
;     - new curve Thatagatha Magic Palm
; version 2.1  by Lasm 2005/17/10 <lasm@rocketmail.com>
;     - new option Bodhidharma's secret manual
; version 2.2  by Lasm 2005/18/10 <lasm@rocketmail.com>
;     - new option 18 weapon
;     - create layer name based on the options selected
; version 3.0  by Lasm 2005/18/10 <lasm@rocketmail.com>
;     - new option Tai Chi curves
; version 3.1  by Lasm 2005/21/10 <lasm@rocketmail.com>
;     - new option Book of Changing the Sinew
;     - new option Wakeup Sermon
;    - optimise 18 Dragon Subduing Palm for Shaolin Line-Art
; version 4.0  by Lasm 2005/27/10 <lasm@rocketmail.com>
;     - new option Yang's Tai Chi Style
; version 4.1  by Lasm 2005/28/10 <lasm@rocketmail.com>
;     - clean up code
; version 4.2  by Lasm 2005/06/11 <lasm@rocketmail.com>
;     - fixed sobel bug
; version 4.3  by Lasm 2005/18/11 <lasm@rocketmail.com>
;     - procedure name hiding
;     - added to SourceForge CVS
; 11/01/2007 - Fixed for gimp 2.4 by Alexia Death
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Instructions on using this script
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Copy this script in the script directory.
; 2. Open up your favourite color photo in Gimp.
; 4. Look for it under Script-Fu->Lasm's FX Effects and fire away !
; 5. For color-punch, copy the new image on top of original color layer
; 6. Slide the opacity level to achieve the desired saturation.
;
; That's all folks. Have fun with this script !
; Another Grandmother Coffee House production.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Notes on Parameter options usage
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Main Art Effects :
; 1.1  Color Punch - Color saturation, strength depends on school
; 1.2  Black & White Beauty - a greyscale rendition
; 1.3  Line-Art Supremo - depends on edge detection plug-ins
; 1.4  Wood-cut clipart - a clip-art effect similar to threshold, but better
; 1.5  Shaolin Line-Art - clean and clear line-art rendition
; 2. Schools :
; 2.1 School affects strength/intensity of all Main Art effects
; 2.2 Buddha Hill Light and Tathagatha are complementary to each other
; 3. Tai Chi Curves:
; 3.1 Tai Chi curves affect the curves adjustment directly
; 4. Options :
; 4.1. kung-fu affects Line-Art Supremo
; 4.1.1 kung-fu on  - cleaner lines, less noise
; 4.1.2 kung-fu off - more texture, more noise
; 4a. BodhiDharma's secret manuals
; 4a.1 Secret Manuals affects all Main Art other than color-punch
; 4.2.1 18-Weapons on - final effect combine to a new layer of original image
; 4.2.2 18-Weapons off - final effect in a separate image
; 4.3.1 Book of Changing the Sinew - Affects 18 Dragon Subduing Palm
; 4.3.2 Wakeup Sermon - Affects Shaolin Line Art
; 5. Yang's Style of Tai Chi 108 Postures
; 5.1 Select various types of edge detection plug-in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Kung-fu trivia
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 72 Shaolin Supreme Skills - the entire set is almost "extinct",
;    mostly lost to current generation, but still being practised    today     NON-FICTION
; 18 Dragon Subduing Palm - from a famous kung-fu novelist's pen, his
;    latest version increases it to 28 different styles                  FICTION
; Half Inch Punch - special skill of late kung-fu star Bruce Lee,
;    no one since then has been able to acquire the same explosive power NON-FICTION
; Thatagatha's Magic Palm - from evergreen kung-fu movie, has 9 levels,
;    the most powerful is the last level.                                FICTION
; Buddha Hill Leg Without Shadow - special skill of legendary hero Wong Fei Hong,
;    who lived in Buddha Hill, Canton, China                      HISTORICAL
; BodhiDharma's secret manuals - Bodhidharma meditated for 9 years inside
;    a Shaolin Cave before he gained Enlightenment                       NON-FICTION
; Book of Changing the Sinew - Attributed to Bodhidharma who taught the
;    monks kung-fu as a form of physical fitness                         NON-FICTION
; Breakthrough Sermon - Short enlightened discourse from Bodhidharama    NON-FICTION
; Wakeup Sermon - Short enlightened discourse from Bodhidharama         NON-FICTION
; 18 Weapons - The total number of long and short Chinese weapons        NON-FICTION
;
;; Set register common information
(define SCRIPT-FU-LABWOW-CHOICE1 (list "Color Punch" "Black & White Beauty" "Line-Art Supremo" "Woodcut Clipart" "Shaolin Line-Art Basic" "Shaolin Line-Art Advanced"))
(define SCRIPT-FU-LABWOW-CHOICE2 (list "72 Supreme Shaolin Skills" "18 Dragon Subduing Palm" "Half Inch Punch" "Tathagata Magic Palm" "Buddha Hill Leg Without Shadow"))
(define SCRIPT-FU-LABWOW-CHOICE3 (list "Sealed Envelope (Edge)" "Playing Pipa" "Fan Through Back" "Left Grasp Swallow Tail" "Right Grasp Swallow Tail" "Single Whip"
                  "Double Wind in Ears (DoG)" "Step to Seven Stars (Laplace)" "Embrace Tiger, Return to Mountain (neon)" "Virtual Dexterity Head Skill (sobel)"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Advanced Photo LAB function:
;
; Requires:
;   plug-in-decompose
;   plug-in-drawable-compose
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-labwow img inLayer stylemode taichi schoolmode yangmode kungfu? secretman? bodhi1? bodhi2? weapon?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Helper function to create kung-fu names and blending options
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (kung-fu-name internal-qi external-qi yangstyle taichi kungfu? secretman? bodhi1? bodhi2?)
  (define wuji (number->string taichi 10))
  (string-append
    (cond
      ((= internal-qi 0) "Color")
      ((= internal-qi 1) "B&W")
      ((= internal-qi 2) "Line_Art")
      ((= internal-qi 3) "Clip_Art")
      ((= internal-qi 4) "Shaolin_Basic")
      ((= internal-qi 5) "Shaolin_Advanced"))
    (cond
      ((= external-qi 0) "-72 Supreme Skills")
      ((= external-qi 1) "-18 Dragon Palm")
      ((= external-qi 2) "-Half Inch")
      ((= external-qi 3) "-Tathagata")
      ((= external-qi 4) "-Buddha"))
    (if (= internal-qi 2)
      (cond
        ((= yangstyle 0) "-yang1")
        ((= yangstyle 1) "-yang2")
        ((= yangstyle 2) "-yang3")
        ((= yangstyle 3) "-yang4")
        ((= yangstyle 4) "-yang5")
        ((= yangstyle 5) "-yang6")
        ((= yangstyle 6) "-yang7")
        ((= yangstyle 7) "-yang8")
        ((= yangstyle 8) "-yang9")
        ((= yangstyle 9) "-yang10")) "")
    "-"
    wuji
    (if (eqv? kungfu? TRUE)
       "-kung fu" "")
    (if (eqv? secretman? TRUE)
       "-secret manual" "")
    (if (eqv? bodhi1? TRUE)
       "-bodhi1" "")
    (if (eqv? bodhi2? TRUE)
       "-bodhi2" "")
   )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Helper function to return curves array
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-lab-curves wuji)
  (let* ((curve-value (cons-array 8 'byte)))
   (aset curve-value 0 0)
   (aset curve-value 1 0)
   (aset curve-value 2 wuji)
   (aset curve-value 3 0)
   (aset curve-value 4 (- 255 wuji))
   (aset curve-value 5 255)
   (aset curve-value 6 255)
   (aset curve-value 7 255)
   curve-value     ; return the curve
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Main function
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (let*
      (
       (width (car (gimp-drawable-width inLayer)))
       (height (car (gimp-drawable-height inLayer)))
       (old-fg (car (gimp-context-get-foreground)))
       (old-bg (car (gimp-context-get-background)))
       (gray-img 0)
       (gray-img2 0)
       (B-layer 0)
       (A-layer 0)
       (L-layer 0)
       (B-layer2 0)
       (A-layer2 0)
       (L-layer2 0)
       (comp-img 0)
       (comp-img2 0)
       (comp-layer 0)
       (last-layer 0)
       (tt-d 0)
       (tt-d2 0)
       (bw-beauty 0)
       (bw-layer 0)
       (bw-layer2 0)
       (bw-layer3 0)
       (bw-layer4 0)
       (bw-layer5 0)
       (bw-layer6 0)
       (bw-beauty2 0)
       (bw-layer-yang7 0)
       (bw-layer-yang9 0)
       (final-layer 0)
       )

  (gimp-image-undo-group-start img)

;; Real work goes in here

   (gimp-image-set-active-layer img inLayer)

    (set! gray-img (car (plug-in-decompose 1 img inLayer "Lab" TRUE)))
    (set! B-layer (car (gimp-image-get-active-layer gray-img)))
    (set! A-layer (- B-layer 1))
    (set! L-layer (- A-layer 1))

      (gimp-image-set-active-layer gray-img B-layer)
    (gimp-curves-spline B-layer 0 8 (get-lab-curves taichi))
    (gimp-curves-spline A-layer 0 8 (get-lab-curves taichi))

    (set! comp-img (car
        (plug-in-drawable-compose RUN-NONINTERACTIVE gray-img L-layer A-layer B-layer 0 "Lab")))
    (set! last-layer (car (gimp-image-get-active-drawable comp-img)))
    (set! tt-d (car (gimp-display-new comp-img))) ;; working

  (if (>= stylemode 1)
    (begin
    (set! comp-layer (car (gimp-image-get-active-layer comp-img)))
    (set! gray-img2 (car (plug-in-decompose RUN-NONINTERACTIVE comp-img comp-layer "Lab" TRUE)))
    (set! B-layer2 (car (gimp-image-get-active-layer gray-img2)))
    (set! A-layer2 (- B-layer2 1))
    (set! L-layer2 (- A-layer2 1))
      (gimp-image-set-active-layer gray-img2 L-layer2)
       (gimp-context-set-background '(128 128 128))
       (gimp-edit-fill A-layer2 BACKGROUND-FILL)
      ;; More b&w tweaking here
    (if (eqv? bodhi1? TRUE)
        (begin
        (if (eqv? bodhi2? TRUE)
            (gimp-curves-spline L-layer2 0 8 (get-lab-curves taichi)))
        (plug-in-c-astretch RUN-NONINTERACTIVE gray-img2 L-layer2)
        (plug-in-normalize RUN-NONINTERACTIVE gray-img2 L-layer2)
        (gimp-equalize L-layer2 FALSE)
            ))
    (set! comp-img2 (car
        (plug-in-drawable-compose RUN-NONINTERACTIVE gray-img2 L-layer2 -1 -1 0 "Lab")))

    (set! bw-layer (car (gimp-image-get-active-layer comp-img2)))
    (set! tt-d2 (car (gimp-display-new comp-img2))) ;; working
    ;;; At this point we have a good black and white image
    ;; Some tweaking below

    (if (eqv? secretman? TRUE)
        (begin
        (gimp-levels-stretch bw-layer)                 ; Auto-levels
        (set! bw-beauty (car(gimp-layer-copy bw-layer 1)))
        (gimp-drawable-set-name bw-beauty "Black White Beauty")
        (gimp-image-add-layer comp-img2 bw-beauty -1)
        (if (= schoolmode 1)
          (plug-in-vinvert 1 comp-img2 bw-layer)
          (if (eqv? kungfu? TRUE)
            (begin
            (set! bw-beauty2 (car(gimp-layer-copy bw-layer 1)))
            (gimp-image-add-layer comp-img2 bw-beauty2 -1)
            (gimp-layer-set-mode bw-beauty2 DIVIDE-MODE)
            ))
          )
        (gimp-layer-set-mode bw-beauty (cond
                            ((= schoolmode 0) SCREEN-MODE)
                            ((= schoolmode 1) DIFFERENCE-MODE) ;; HARDLIGHT
                            ((= schoolmode 2) GRAIN-MERGE-MODE)
                            ((= schoolmode 3) ADDITION-MODE)
                            ((= schoolmode 4) DODGE-MODE)))
        (set! bw-layer (car (gimp-image-merge-visible-layers comp-img2 0)))
        ))
      ))

  (if (= stylemode 2)
    (begin
    (cond
       ((= yangmode 0)  (plug-in-edge RUN-NONINTERACTIVE comp-img2 bw-layer 2 1 0))
       ((= yangmode 1)  (plug-in-edge RUN-NONINTERACTIVE comp-img2 bw-layer 2 1 1))
       ((= yangmode 2)  (plug-in-edge RUN-NONINTERACTIVE comp-img2 bw-layer 2 1 2))
       ((= yangmode 3)  (plug-in-edge RUN-NONINTERACTIVE comp-img2 bw-layer 2 1 3))
       ((= yangmode 4)  (plug-in-edge RUN-NONINTERACTIVE comp-img2 bw-layer 2 1 4))
       ((= yangmode 5)  (plug-in-edge RUN-NONINTERACTIVE comp-img2 bw-layer 2 1 5))
       ((= yangmode 6)  (plug-in-dog RUN-NONINTERACTIVE comp-img2 bw-layer 3 1 TRUE TRUE))
       ((= yangmode 7)  (begin
                  (if (eqv? bodhi1? TRUE)
                    (plug-in-sel-gauss RUN-NONINTERACTIVE comp-img2 bw-layer 5 50))
                  (set! bw-layer-yang7 (car(gimp-layer-copy bw-layer 1)))
                  (gimp-image-add-layer comp-img2 bw-layer-yang7 -1)
                  (gimp-context-set-background '(255 255 255))
                     (gimp-edit-fill bw-layer BG-IMAGE-FILL)
                  (plug-in-laplace RUN-NONINTERACTIVE comp-img2 bw-layer-yang7)
                  (set! bw-layer (car (gimp-image-merge-down comp-img2 bw-layer-yang7 0)))
                  ))
       ((= yangmode 8)  (plug-in-neon RUN-NONINTERACTIVE comp-img2 bw-layer 5 0))
       ((= yangmode 9)  (begin
                  (set! bw-layer-yang9 (car(gimp-layer-copy bw-layer 1)))
                  (gimp-image-add-layer comp-img2 bw-layer-yang9 -1)
                  (gimp-context-set-background '(255 255 255))
                     (gimp-edit-fill bw-layer BG-IMAGE-FILL)
                  (plug-in-sobel RUN-NONINTERACTIVE comp-img2 bw-layer-yang9 TRUE TRUE TRUE)
                  (set! bw-layer (car (gimp-image-merge-down comp-img2 bw-layer-yang9 0)))
                  ))
       )
    (if (<= yangmode 5)
       (begin
       (gimp-posterize bw-layer (if (eqv? kungfu? TRUE) 2 3))
       (plug-in-vinvert 1 comp-img2 bw-layer)))
    (if (= yangmode 8)
       (gimp-invert bw-layer))
    (if (eqv? bodhi2? TRUE)
       (plug-in-dilate RUN-NONINTERACTIVE comp-img2 bw-layer 0 0 0.5 7 0 128))
    ))

  (if (>= stylemode 3)
    (begin
    (gimp-posterize bw-layer 2)
    (plug-in-vinvert 1 comp-img2 bw-layer)  ; get rid of color abberations
    (plug-in-vinvert 1 comp-img2 bw-layer)  ; introduced by secretman
    ))

  (if (>= stylemode 4)
    (begin
    (set! bw-layer2 (car(gimp-layer-copy bw-layer 1)))
      (gimp-drawable-set-name bw-layer2 "Shaolin Layer")
    (gimp-image-add-layer comp-img2 bw-layer2 -1)

    (if (eqv? bodhi2? TRUE)
        (begin
           (gimp-context-set-background '(0 0 0))
           (gimp-edit-fill bw-layer2 BG-IMAGE-FILL)
           (gimp-by-color-select bw-layer '(255 255 255) 15 2 TRUE TRUE (if (eqv? kungfu? FALSE) 2 3) FALSE)
          (gimp-context-set-background '(255 255 255)))
        (begin
           (gimp-context-set-background '(255 255 255))
           (gimp-edit-fill bw-layer2 BG-IMAGE-FILL)
           (gimp-by-color-select bw-layer '(0 0 0) 15 2 TRUE TRUE (if (eqv? kungfu? FALSE) 2 3) FALSE)
          (gimp-context-set-background '(0 0 0)))
        )
       (gimp-edit-fill bw-layer2 BG-IMAGE-FILL)
    (gimp-selection-none comp-img2)
    (gimp-layer-set-mode bw-layer2 DIFFERENCE-MODE)
    (set! bw-layer3 (car (gimp-image-merge-visible-layers comp-img2 0)))
    (gimp-invert bw-layer3)
    (gimp-drawable-set-name bw-layer3 "Shaolin Line-Art Basic")
    ))

  (if (= stylemode 5)
    (begin
    (set! bw-layer4 (car(gimp-layer-copy bw-layer3 1)))
      (gimp-drawable-set-name bw-layer4 "Shaolin Layer")
    (gimp-image-add-layer comp-img2 bw-layer4 -1)
    (gimp-layer-set-mode bw-layer4 MULTIPLY-MODE)

    (set! bw-layer5 (car(gimp-layer-copy bw-layer4 1)))
      (gimp-drawable-set-name bw-layer5 "Shaolin Layer")
    (gimp-image-add-layer comp-img2 bw-layer5 -1)
    (gimp-layer-set-mode bw-layer5 MULTIPLY-MODE)

    (set! bw-layer6 (car (gimp-image-merge-visible-layers comp-img2 0)))
    (gimp-drawable-set-name bw-layer6 "Shaolin Line-Art Advanced")
    (plug-in-unsharp-mask RUN-NONINTERACTIVE comp-img2 bw-layer6 2 4 4)
    ))
;; Create kung-fu layer name and combine to original image
  (if (= stylemode 0)
    (begin
        (if (eqv? weapon? TRUE)
        (begin
          (set! final-layer (car (gimp-layer-new-from-drawable last-layer img)))
          (gimp-image-add-layer img final-layer -1)
          (gimp-drawable-set-name final-layer (kung-fu-name stylemode schoolmode yangmode taichi kungfu? secretman? bodhi1? bodhi2?))
            (gimp-display-delete tt-d))
          (begin
          (gimp-drawable-set-name last-layer (kung-fu-name stylemode schoolmode yangmode taichi kungfu? secretman? bodhi1? bodhi2?)))
        ))
    (begin
      (set! last-layer (car (gimp-image-get-active-drawable comp-img2)))
        (if (eqv? weapon? TRUE)
        (begin
          (set! final-layer (car (gimp-layer-new-from-drawable last-layer img)))
          (gimp-image-add-layer img final-layer -1)
          (gimp-drawable-set-name final-layer (kung-fu-name stylemode schoolmode yangmode taichi kungfu? secretman? bodhi1? bodhi2?))
        (gimp-display-delete tt-d2))
          (begin
          (gimp-drawable-set-name last-layer (kung-fu-name stylemode schoolmode yangmode taichi kungfu? secretman? bodhi1? bodhi2?)))
      ))
  )
;; clean up before exit
  (gimp-image-delete gray-img)
  (if (>= stylemode 1)
    (begin
      (gimp-display-delete tt-d)
        (gimp-image-delete gray-img2))
  )

  (gimp-image-undo-group-end img)
  (gimp-displays-flush)

  (gimp-context-set-background old-bg)
  (gimp-context-set-foreground old-fg)

  )
)


(script-fu-register
 "script-fu-labwow"
 _"<Image>/FX-Foundry/Color/Lasm's LAB Wow"
 "Version 4.3 \nLasm's famous special effect for photographs. This works on any RGB image. Caution: it runs slowly on large images.\n
  Shaolin Line Art works best on sharp macro photos with high contrast. Try using various options in 18 Dragon Subduing Palm"
 "lasm"
 "Copyright 2005, lasm"
 "Oct 5, 2005"
 "RGB*"
 SF-IMAGE              "The Image"                       0
 SF-DRAWABLE              "The Layer"                       0
 SF-OPTION            _"Main Art Style"                  SCRIPT-FU-LABWOW-CHOICE1
 SF-ADJUSTMENT        _"Tai Chi Curves"                '(42 1 127 1 10 0 0)
 SF-OPTION            _"School"                          SCRIPT-FU-LABWOW-CHOICE2
 SF-OPTION            _"Yang's Style 108 Postures(For Line-Art Supremo only)"     SCRIPT-FU-LABWOW-CHOICE3
 SF-TOGGLE            _"Kung-fu"                         FALSE
 SF-TOGGLE            _"BodhiDharma's Secret Manual ?"    FALSE
 SF-TOGGLE            _"Book of Changing the Sinew"        FALSE
 SF-TOGGLE            _"Wakeup Sermon"                FALSE
 SF-TOGGLE            _"18 Weapons"                     TRUE
)

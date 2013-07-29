; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Convolution Matrix Presets script  for GIMP 2.4
; Copyright (C) 2001-2002 Iccii <iccii@hotmail.com>
;
; Tags: convolution, presets, filter
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; version 0.1  2001/09/30 Iccii <iccii@hotmail.com>
;     - Initial relase (has not worked yet!)
; version 0.1a 2001/10/04 Iccii <iccii@hotmail.com>
;     - Now, real initial relase (good work!) Thanks, Kajiyama
; version 0.2  2001/10/04 Iccii <iccii@hotmail.com>
;     - Divided into multi scripts for each effects
; version 0.3  2001/10/06 Iccii <iccii@hotmail.com>
;     - Now, you can control any amount of effects by adjusting Amount slider
; version 0.3a 2001/10/19 Iccii <iccii@hotmail.com>
;     - Minor bug fixes and added some types in Vibration
; version 0.4  2001/10/19 Iccii <iccii@hotmail.com>
;     - Defined the get-common-matrix-list function
;     - Defined the get-around-number function
; version 0.4a 2001/10/20 Iccii <iccii@hotmail.com>
;     - Added Angle and Desaturate option in Emboss
; version 0.4b 2001/11/11 Iccii <iccii@hotmail.com>
;     - Fixed small wrong parameter setting
;     - Added some convolution type
;     - Added Color option in Emboss
; version 0.5  2001/11/14 lasm <lasm@rocketmail.com>
;     - Added 3 more types and reorder types
;     - Added 4 new symmetric presets
;     - Deleted adjustable blur
; version 0.5a 2001/12/14 Iccii <iccii@hotmail.com>
;     - Added some types in Others
; version 0.6  2002/01/19 Iccii <iccii@hotmail.com>
;     - Small changes (delete get-around-number function)
;     - Fixed bug in Edge-Blur
;     - Added new edge-detect mode (in Edge detect and Symmetric
;       Edge Detect)
;     - Rename Posterize to Posterize Variation
;     - ...and added new posterize effects
;     - Corrected many typos ;)
; version 0.6a 2002/01/26 Iccii <iccii@hotmail.com>
;     - Added Laplace Edge-Eetect (algorithm from laplace.c source)
;     - Added Edge-Detect (Blur) which uses blur
; version 0.7  2002/01/27 Iccii <iccii@hotmail.com>
;     - Merged convolution-preset2.scm into this script
;     - Added convlution-preset-type-is-*
;     - Added Unsharp Mask filter (algorithm from unsharp-mask.scm)
;     - Added refarence web site (HIPR2)
; version 0.7a 2002/01/29 Iccii <iccii@hotmail.com>
;     - Use gimp-image-merge-visible-layers instead of
;       gimp-image-merge-down
;     - Added new blur option (Mean Filter and Median Filter)
; version 0.7b 2002/02-01 Iccii <iccii@hotmail.com>
;     - Added Prewitt and Roberts and Liner Difference filter option
;       in script-fu-convolution-presets-edge-detect-sobel
;     - Added Gaussian Laplacian option in
;       script-fu-convolution-presets-edge-detect-laplace
; version 0.8  2002/02/02 Iccii <iccii@hotmail.com>
;     - Added two edge detector:
;       script-fu-convolution-presets-edge-detect-compass
;       script-fu-convolution-presets-edge-detect-line
;     - Removed Median Filter
; version 0.8a 2002/02/03 Iccii <iccii@hotmail.com>
;     - Added script-fu-convolution-presets-blur-gaussian
;     - Remove Gaussian Laplacian option into
;       Pre-appling Gaussian Blur
; version 0.8b 2002/02/05 Iccii <iccii@hotmail.com>
;     - Added function name list in comment area
;     - Added Cross (+,x) Angle option in Edge Detect (Line)
;     - New function: script-fu-convolution-presets-emboss-standard
; version 0.8c 2002/02/07 Iccii <iccii@hotmail.com>
;     - Fixed bug (Blur Gaussian was not shown menu list)
;     - Added new "More Strong" option in Emboss Standard
; version 0.9  2002/02/25 Iccii <iccii@hotmail.com>
;     - Added "Brightness" option in Posterize
;     - Added "Motion Blur" toggle in Vibration
;     - Added script-fu-convolution-presets-shake
;     - Added Invert? and Color Mode options in edge-detect-laplace
;     - Added Invert? option in edge-detect-sobel
;     - Added Gradient option for Filter Type in edge-detect-sobel
;     - Added script-fu-convolution-presets-edge-detect-prewitt
;     - Added script-fu-convolution-presets-artistic
;     - Added new helper function: get-rotated-matrix-list
;     - Changed accordingly: emboss-variation
;     - Added new variation type in emboss-variation
;     - Enabled adjustment for effects in emboss-variation
;     - s/local-debug-output/conv-preset-debug-output/
;     - Added script-fu-convolution-presets-brightness-contrast
; version 0.9a 2002/03/06 Iccii <iccii@hotmail.com>
;     - Added script-fu-convolution-presets-smudge
;version 1.0 2004/09 Raymond Ostertag
;     - Ported to Gimp2
;version 1.1 2007/10 Alexia Death
;     - Fixed for Gimp 2.4
;version 1.2 2007/10 Alexia Death
;     - Fixed som global variable issues, more to do, prepped for inclusion in FX foundry
; --------------------------------------------------------------------
;     Reference Book
; http://bookweb.kinokuniya.co.jp/cgi-bin/gwfind.cgi?W-NIPS=9971628929
; --------------------------------------------------------------------
;     Refarence Web Site
; http://www.dai.ed.ac.uk/HIPR2/hipr_top.htm
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
;    ----- function list available in this file -----
;
;    Symmetric
;
; script-fu-convolution-presets-blur-symmetric
;            img drawable
;            type a c calc_divisor?
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-edge-symmetric
;            img drawable
;            type a mode calc_divisor?
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-sharp-symmetric
;            img drawable
;            type a c calc_divisor?
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-general-symmetric
;            img drawable
;            type a c offset calc_divisor?
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
;    Sharp
;
; script-fu-convolution-presets-sharp
;            img drawable
;            type a
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-sharp-adjustable
;            img drawable
;            type a c
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-unsharp-mask
;            img drawable
;            type a mask-opacity
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
;    Blur
;
; script-fu-convolution-presets-blur
;            img drawable
;            type a filter-type
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-blur-adjustable
;            img drawable
;            type a c
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-blur-gaussian
;            img drawable
;            type a filter-type radius
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
;    Edge detection
;
; script-fu-convolution-presets-edge-detect
;            img drawable
;            type a mode
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-edge-detect-laplace
;            img drawable
;            type a color-mode gauss? invert? desaturate?
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-edge-detect-sobel
;            img drawable
;            layer-mode a filter-type invert? desaturate?
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-edge-detect-prewitt
;            img drawable
;            a color-mode invert? desaturate?
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-edge-detect-line
;            img drawable
;            a angle color-mode desaturate?
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-edge-detect-compass
;            img drawable
;            a angle color-mode desaturate?
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-edge-detect-blur
;            img drawable
;            type a detect-type blur-type desaturate?
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
;    Vibration
;
; script-fu-convolution-presets-vibration
;            img drawable
;            type a motion-blur?
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
;    Emboss
;
;(define (script-fu-convolution-presets-emboss-standard
;            img drawable
;            desaturate-type type a strong? flip?
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-emboss
;            img drawable
;            type desaturate-type angle a
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-emboss-variation
;            img drawable
;            type degree
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
;    Brightness Contrast
;
; script-fu-convolution-presets-brightness-contrast
;            img drawable
;            brightness contrast
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
;    Posterize
;
; script-fu-convolution-presets-posterize
;            img drawable
;            a brightness
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
; script-fu-convolution-presets-posterize-variation
;            img drawable
;            type a
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
;    Shake
;
;(define (script-fu-convolution-presets-shake
;            img drawable
;            type a invert?
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
;    Color invert
;
; script-fu-convolution-presets-color-invert
;            img drawable
;            type a
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
;    Artistic
;
; script-fu-convolution-presets-artistic
;            img drawable
;            type a detail-level blur?
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
;    Smudge (Mean Filter)
;
; script-fu-convolution-presets-smudge
;            img drawable
;            type a repeat
;            alpha_alg? b_mod
;            gray? red? green? blue? alpha?)
;
;    Others
;
; script-fu-convolution-presets-others
;            img drawable
;            type
;            alpha_alg? b_mode
;            gray? red? green? blue? alpha?
;
;    (test)
;
; script-fu-convolution-presets-random-color
;            img drawable
;            type a
;            b_mode
;
; script-fu-convolution-presets-effects-test
;            img drawable
;            type a effects-type
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Rescan script-fu (or restart GIMP) if you change this setting

    ;; define one of three to #t

(define convolution-preset-type-is-basic    #t)
(define convolution-preset-type-is-standard #f)
(define convolution-preset-type-is-advanced #f)

    ;; define #t or #f (don't use #t because it will not work yet)

(define convolution-preset-default-setting #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Set register common information (global stuff)

    (define CONV-PRESET-DESCRIPTION "Set of presets for convolution matrix ")
    (define CONV-PRESET-COPYRIGHT   "Iccii")
    (define CONV-PRESET-DATE        "Feb, 2002")
    (define CONV-PRESET-IMAGE-TYPE  "RGB* GRAY*")
    (define CONV-PRESET-EDGE-LIST   (list "Extend" "Wrap" "Crop"))

        ;; see get-rotated-matrix-list
    (define CONV-PRESET-DEGREE-LIST '("  0" " 45" " 90" "135"
                    "180" "225" "270" "315"
                    " 30 (unrecommend)" " 60 (unrecommend)"
                    "120 (unrecommend)" "150 (unrecommend)"
                    "210 (unrecommend)" "240 (unrecommend)"
                    "300 (unrecommend)" "330 (unrecommend)"))
        ;; see get-common-matrix-list
    (define CONV-PRESET-TYPE-LIST (cond
    (convolution-preset-type-is-basic
    (list "Small" "Medium1" "Medium2" "Large"))
    (convolution-preset-type-is-standard
    (list "Type  1  (Small)" "Type  2  (Small)" "Type  3  (Small)"
        "Type  4  (Small)" "Type  5  (Small)" "Type  6  (Small)"
        "Type  7 (Medium)" "Type  8 (Medium)" "Type  9 (Medium)"
        "Type 10 (Medium)" "Type 11 (Medium)" "Type 12 (Medium)"
        "Type 13 (Strong)" "Type 14 (Strong)" "Type 15 (Strong)"
        "Type 16 (Strong)" "Type 17 (Strong)" "Type 18 (Strong)"))
    (convolution-preset-type-is-advanced
    (list "Type  1" "Type  2" "Type  3" "Type  4" "Type  5"
        "Type  6" "Type  7" "Type  8" "Type  9" "Type 10"
        "Type 11" "Type 12" "Type 13" "Type 14" "Type 15"
        "Type 16" "Type 17" "Type 18" "Type 19" "Type 20"
        "Type 21" "Type 22" "Type 23" "Type 24" "Type 25"
        "Type 26" "Type 27" "Type 28" "Type 29" "Type 30"
        "Type 31" "Type 32" "Type 33"))
    )) ; end of set! CONV-PRESET-TYPE-LIST

(if convolution-preset-default-setting

    (define alpha_alg? TRUE)
    (define b_mode     0)        ; border mode is Extend
    (define gray?      TRUE)
    (define red?       TRUE)
    (define green?     TRUE)
    (define blue?      TRUE)
    (define alpha?     FALSE)
)

    ;; Set argcs

(define argc_matrix   25)
(define argc_channles  5)


    ;; Check & Return alpha_alg and bmode

(define (get-alpha_alg-bmode drawable alpha_alg? bmode)
  (if (eqv? (car (gimp-drawable-has-alpha drawable)) TRUE)
      (list (if (eqv? alpha_alg? TRUE) 1 0) bmode) ; Return (drawable has alpha)
      (list 0 (if (= bmode 2) 0 bmode))))          ; Return (drawable has no alpha)


    ;; Check channels and return channel array

(define (get-channels drawable gray? red? green? blue? alpha?)
  (let* ((channels (cons-array argc_channles 'long))
     (red 0)
         (green 0)
         (blue  0)
         (alpha  0)
         (gray  0)

    )

    (if (eqv? (car (gimp-drawable-is-gray drawable)) TRUE)
        (begin
            (set! gray (if (eqv? gray? TRUE) 1 0))
                (set! gray 0)
    )
    )
    (if (eqv? (car (gimp-drawable-is-rgb drawable)) TRUE)
        (begin
          (set! red   (if (eqv? red?   TRUE) 1 0))
          (set! green (if (eqv? green? TRUE) 1 0))
          (set! blue  (if (eqv? blue?  TRUE) 1 0)))
        (begin
          (set! red   0)
          (set! green 0)
          (set! blue  0)))
    (if (eqv? (car (gimp-drawable-has-alpha drawable)) TRUE)
      (begin
        (set! alpha   (if (eqv? alpha? TRUE) 1 0))
        (set! alpha   0)))
      (aset channels 0 gray)
      (aset channels 1 red)
      (aset channels 2 green)
      (aset channels 3 blue)
      (aset channels 4 alpha)
    channels)) ; Return the channel array


    ;; Convert matrix list (25) into matrix array (5x5)

(define (get-matrix matrix-list)
  (define (list-ref l n) (nth n l))
  (let* ((count 0)
         (matrix (cons-array argc_matrix 'double)))
    (while (< count argc_matrix)
      (aset matrix count (list-ref matrix-list count))
      (set! count (+ count 1)))
    matrix)) ; Return the matrix array


    ;; (conv-preset-debug-output matrix channels alpha_alg bmode)
    ;; insert to debug

(define (conv-preset-debug-output matrix channels alpha_alg bmode)
  (let* ((message "5x5 Matrix:\n\n")
         (count 0))
    (while (< count argc_matrix)
      (set! message (string-append message " "
                                   (number->string (aref matrix count))))
      (set! count (+ count 1))
      (if (or (= count 5) (= count 10) (= count 15) (= count 20))
          (set! message (string-append message "\n"))))
    (gimp-message message))

  (let* ((message "Channels\nGray Red Green Blue Alpha:\n")
         (count 0))
    (while (< count argc_channles)
      (set! message (string-append message " "
                                   (number->string (aref channels count))))
      (set! count (+ count 1)))
    (gimp-message (string-append message "\n\n"
                                 "alpha_alg: " (number->string alpha_alg) "\n"
                                 "bmode: "     (number->string bmode)))  ))


    ;; Rotate matrix list

(define (get-rotated-matrix-list matrix-list degree)
  (define (list-ref l n) (nth n l))
  (let* ((x11 (list-ref matrix-list 0))
         (x12 (list-ref matrix-list 1))
         (x13 (list-ref matrix-list 2))
         (x14 (list-ref matrix-list 3))
         (x15 (list-ref matrix-list 4))
         (x21 (list-ref matrix-list 5))
         (x22 (list-ref matrix-list 6))
         (x23 (list-ref matrix-list 7))
         (x24 (list-ref matrix-list 8))
         (x25 (list-ref matrix-list 9))
         (x31 (list-ref matrix-list 10))
         (x32 (list-ref matrix-list 11))
         (x33 (list-ref matrix-list 12))
         (x34 (list-ref matrix-list 13))
         (x35 (list-ref matrix-list 14))
         (x41 (list-ref matrix-list 15))
         (x42 (list-ref matrix-list 16))
         (x43 (list-ref matrix-list 17))
         (x44 (list-ref matrix-list 18))
         (x45 (list-ref matrix-list 19))
         (x51 (list-ref matrix-list 20))
         (x52 (list-ref matrix-list 21))
         (x53 (list-ref matrix-list 22))
         (x54 (list-ref matrix-list 23))
         (x55 (list-ref matrix-list 24))
         (x00 (/ (+ x22 x23) 2))    ; x11     x12     x13     x14     x15
         (x01 (/ (+ x23 x24) 2))    ;      \           |           /
         (x02 (/ (+ x24 x34) 2))    ; x21     x22 x00 x23 x01 x24     x25
         (x03 (/ (+ x34 x44) 2))    ;         x07  \   |   /  x02
         (x04 (/ (+ x44 x43) 2))    ; x31 --- x32 --- x33 --- x34 --- x35
         (x05 (/ (+ x43 x42) 2))    ;         x06  /   |   \  x03
         (x06 (/ (+ x42 x32) 2))    ; x41     x42 x05 x43 x04 x44     x45
         (x07 (/ (+ x32 x22) 2))    ;      /           |           \
    )                               ; x51     x52     x53     x54     x55
    (cond
      ((or (= degree 0) (= degree 0))
        (list x11 x12 x13 x14 x15
              x21 x22 x23 x24 x25
              x31 x32 x33 x34 x35
              x41 x42 x43 x44 x45
              x51 x52 x53 x54 x55))
      ((or (= degree 1) (= degree 45))
        (list x13 x14 x15 x25 x35
              x12 x23 x24 x34 x45
              x11 x22 x33 x44 x55
              x21 x32 x42 x43 x54
              x31 x41 x51 x52 x53))
      ((or (= degree 2) (= degree 90))
        (list x15 x25 x35 x45 x55
              x14 x24 x34 x44 x54
              x13 x23 x33 x43 x53
              x12 x22 x32 x42 x52
              x11 x21 x31 x41 x51))
      ((or (= degree 3) (= degree 135))
        (list x35 x45 x55 x54 x53
              x25 x34 x44 x43 x52
              x15 x24 x33 x42 x51
              x14 x23 x22 x32 x41
              x13 x12 x11 x21 x31))
      ((or (= degree 4) (= degree 180))
        (list x55 x54 x53 x52 x51
              x45 x44 x43 x42 x41
              x35 x34 x33 x32 x31
              x25 x24 x23 x22 x21
              x15 x14 x13 x12 x11))
      ((or (= degree 5) (= degree 225))
        (list x53 x52 x51 x41 x31
              x54 x43 x42 x32 x21
              x55 x44 x33 x22 x11
              x45 x34 x24 x23 x12
              x35 x25 x15 x14 x13))
      ((or (= degree 6) (= degree 270))
        (list x51 x41 x31 x21 x11
              x52 x42 x32 x22 x12
              x53 x43 x33 x23 x13
              x54 x44 x34 x24 x14
              x55 x45 x35 x25 x15))
      ((or (= degree 7) (= degree 315))
        (list x31 x21 x11 x12 x13
              x41 x32 x22 x23 x14
              x51 x42 x33 x24 x15
              x52 x43 x44 x34 x25
              x53 x54 x55 x45 x35))
      ((or (= degree 8) (= degree 30))
        (list x12 x13 x14 x15 x25
              x11 x00 x01 x02 x35
              x21 x07 x33 x03 x45
              x31 x06 x05 x04 x55
              x41 x51 x52 x53 x54))
      ((or (= degree 9) (= degree 60))
        (list x14 x15 x25 x35 x45
              x13 x01 x02 x03 x55
              x12 x00 x33 x04 x54
              x11 x07 x06 x05 x53
              x21 x31 x41 x51 x52))
      ((or (= degree 10) (= degree 120))
        (list x25 x35 x45 x55 x54
              x15 x02 x03 x04 x53
              x14 x01 x33 x05 x52
              x13 x00 x07 x06 x51
              x12 x11 x21 x31 x41))
      ((or (= degree 11) (= degree 150))
        (list x45 x55 x54 x53 x52
              x35 x03 x04 x05 x51
              x25 x02 x33 x06 x41
              x15 x01 x00 x07 x31
              x14 x13 x12 x11 x21))
      ((or (= degree 12) (= degree 210))
        (list x54 x53 x52 x51 x41
              x55 x04 x05 x06 x31
              x45 x03 x33 x07 x21
              x35 x02 x01 x00 x11
              x25 x15 x14 x13 x12))
      ((or (= degree 13) (= degree 240))
        (list x52 x51 x41 x31 x21
              x53 x05 x06 x07 x11
              x54 x04 x33 x00 x12
              x55 x03 x02 x01 x13
              x45 x35 x25 x15 x14))
      ((or (= degree 14) (= degree 300))
        (list x41 x31 x21 x11 x12
              x51 x06 x07 x00 x13
              x52 x05 x33 x01 x14
              x53 x04 x03 x02 x15
              x54 x55 x45 x35 x25))
      ((or (= degree 15) (= degree 330))
        (list x21 x11 x12 x13 x14
              x31 x07 x00 x01 x15
              x41 x06 x33 x02 x25
              x51 x05 x04 x03 x35
              x52 x53 x54 x55 x45))
    )))


    ;; Return common matrix list

(define (get-common-matrix-list type b c) (cond

  (convolution-preset-type-is-basic        ;;;;;;;;;; basic
    (cond
      ((= type 0)
        (list  0  0  0  0  0
               0  0  b  0  0
               0  b  c  b  0
               0  0  b  0  0
               0  0  0  0  0    4))
      ((= type 1)
        (list  0  0  0  0  0
               0  b  b  b  0
               0  b  c  b  0
               0  b  b  b  0
               0  0  0  0  0    8))
      ((= type 2)
        (list  0  0  b  0  0
               0  b  b  b  0
               b  b  c  b  b
               0  b  b  b  0
               0  0  b  0  0    12))
      ((= type 3)
        (list  b  b  b  b  b
               b  b  b  b  b
               b  b  c  b  b
               b  b  b  b  b
               b  b  b  b  b    24))))

  (convolution-preset-type-is-standard        ;;;;;;;;;; standard
    (cond
      ((= type 0)
        (list  0  0  0  0  0
               0  0  0  0  0
               0  b  c  b  0
               0  0  0  0  0
               0  0  0  0  0    2))
      ((= type 1)
        (list  0  0  0  0  0
               0  b  0  0  0
               0  0  c  0  0
               0  0  0  b  0
               0  0  0  0  0    2))
      ((= type 2)
        (list  0  0  0  0  0
               0  0  b  0  0
               0  0  c  0  0
               0  0  b  0  0
               0  0  0  0  0    2))
      ((= type 3)
        (list  0  0  0  0  0
               0  0  0  b  0
               0  0  c  0  0
               0  b  0  0  0
               0  0  0  0  0    2))
      ((= type 4)
        (list  0  0  b  0  0
               0  0  0  0  0
               b  0  c  0  b
               0  0  0  0  0
               0  0  b  0  0    4))
      ((= type 5)
        (list  b  0  0  0  b
               0  0  0  0  0
               0  0  c  0  0
               0  0  0  0  0
               b  0  0  0  b    4))
      ((= type 6)
        (list  0  0  0  0  0
               0  0  b  0  0
               0  b  c  b  0
               0  0  b  0  0
               0  0  0  0  0    4))
      ((= type 7)
        (list  0  0  0  0  0
               0  b  0  b  0
               0  0  c  0  0
               0  b  0  b  0
               0  0  0  0  0    4))
      ((= type 8)
        (list  b  0  b  0  b
               0  0  0  0  0
               b  0  c  0  b
               0  0  0  0  0
               b  0  b  0  b    8))
      ((= type 9)
        (list  0  b  0  b  0
               b  0  0  0  b
               0  0  c  0  0
               b  0  0  0  b
               0  b  0  b  0    8))
      ((= type 10)
        (list  0  0  b  0  0
               0  0  b  0  0
               b  b  c  b  b
               0  0  b  0  0
               0  0  b  0  0    8))
      ((= type 11)
        (list  b  0  0  0  b
               0  b  0  b  0
               0  0  c  0  0
               0  b  0  b  0
               b  0  0  0  b    8))
      ((= type 12)
        (list  0  0  0  0  0
               0  b  b  b  0
               0  b  c  b  0
               0  b  b  b  0
               0  0  0  0  0    8))
      ((= type 13)
        (list  b  0  b  0  b
               0  0  b  0  0
               b  b  c  b  b
               0  0  b  0  0
               b  0  b  0  b    12))
      ((= type 14)
        (list  0  0  b  0  0
               0  b  b  b  0
               b  b  c  b  b
               0  b  b  b  0
               0  0  b  0  0    12))
      ((= type 15)
        (list  b  b  b  b  b
               b  0  0  0  b
               b  0  c  0  b
               b  0  0  0  b
               b  b  b  b  b    16))
      ((= type 16)
        (list  b  0  b  0  b
               0  b  b  b  0
               b  b  c  b  b
               0  b  b  b  0
               b  0  b  0  b    16))
      ((= type 17)
        (list  b  b  b  b  b
               b  b  b  b  b
               b  b  c  b  b
               b  b  b  b  b
               b  b  b  b  b    24))
      ('else    ; otherwise (error?)
        (list  0  0  0  0  0
               0  0  0  0  0
               0  0  1  0  0
               0  0  0  0  0
               0  0  0  0  0    0))))

  (convolution-preset-type-is-advanced        ;;;;;;;;;; advanced
    (set! MAX-TYPE 33)
    (cond
      ((= type 0)
        (list  b  b  b  b  b
               b  b  b  b  b
               b  b  c  b  b
               b  b  b  b  b
               b  b  b  b  b    24))

      ((= type 1)
        (list  0  0  0  0  0
               0  0  b  0  0
               0  b  c  b  0
               0  0  b  0  0
               0  0  0  0  0     4))
      ((= type (- MAX-TYPE 1))
        (list  b  b  b  b  b
               b  b  0  b  b
               b  0  c  0  b
               b  b  0  b  b
               b  b  b  b  b    20))
      ((= type 2)
        (list  0  0  0  0  0
               0  b  0  b  0
               0  0  c  0  0
               0  b  0  b  0
               0  0  0  0  0    4))
      ((= type (- MAX-TYPE 2))
        (list  b  b  b  b  b
               b  0  b  0  b
               b  b  c  b  b
               b  0  b  0  b
               b  b  b  b  b    20))
      ((= type 3)
        (list  0  0  b  0  0
               0  0  0  0  0
               b  0  c  0  b
               0  0  0  0  0
               0  0  b  0  0    4))
      ((= type (- MAX-TYPE 3))
        (list  b  b  0  b  b
               b  b  b  b  b
               0  b  c  b  0
               b  b  b  b  b
               b  b  0  b  b    20))
      ((= type 4)
        (list  b  0  0  0  b
               0  0  0  0  0
               0  0  c  0  0
               0  0  0  0  0
               b  0  0  0  b    4))
      ((= type (- MAX-TYPE 4))
        (list  0  b  b  b  0
               b  b  b  b  b
               b  b  c  b  b
               b  b  b  b  b
               0  b  b  b  0    20))

      ((= type 5)
        (list  0  0  0  0  0
               0  b  b  b  0
               0  b  c  b  0
               0  b  b  b  0
               0  0  0  0  0    8))
      ((= type (- MAX-TYPE 5))
        (list  b  b  b  b  b
               b  0  0  0  b
               b  0  c  0  b
               b  0  0  0  b
               b  b  b  b  b    16))
      ((= type 6)
        (list  0  0  b  0  0
               0  b  0  b  0
               b  0  c  0  b
               0  b  0  b  0
               0  0  b  0  0    8))
      ((= type (- MAX-TYPE 6))
        (list  b  b  0  b  b
               b  0  b  0  b
               0  b  c  b  0
               b  0  b  0  b
               b  b  0  b  b    16))
      ((= type 7)
        (list  b  0  0  0  b
               0  0  b  0  0
               0  b  c  b  0
               0  0  b  0  0
               b  0  0  0  b    8))
      ((= type (- MAX-TYPE 7))
        (list  0  b  b  b  0
               b  b  0  b  b
               b  0  c  0  b
               b  b  0  b  b
               0  b  b  b  0    16))
      ((= type 8)
        (list  0  0  b  0  0
               0  0  b  0  0
               b  b  c  b  b
               0  0  b  0  0
               0  0  b  0  0    8))
      ((= type (- MAX-TYPE 8))
        (list  b  b  0  b  b
               b  b  0  b  b
               0  0  c  0  0
               b  b  0  b  b
               b  b  0  b  b    16))
      ((= type 9)
        (list  b  0  0  0  b
               0  b  0  b  0
               0  0  c  0  0
               0  b  0  b  0
               b  0  0  0  b    8))
      ((= type (- MAX-TYPE 9))
        (list  0  b  b  b  0
               b  0  b  0  b
               b  b  c  b  b
               b  0  b  0  b
               0  b  b  b  0    16))
      ((= type 10)
        (list  0  b  0  b  0
               b  0  0  0  b
               0  0  c  0  0
               b  0  0  0  b
               0  b  0  b  0    8))
      ((= type (- MAX-TYPE 10))
        (list  b  0  b  0  b
               0  b  b  b  0
               b  b  c  b  b
               0  b  b  b  0
               b  0  b  0  b    16))
      ((= type 11)
        (list  b  0  b  0  b
               0  0  0  0  0
               b  0  c  0  b
               0  0  0  0  0
               b  0  b  0  b    8))
      ((= type (- MAX-TYPE 11))
        (list  0  b  0  b  0
               b  b  b  b  b
               0  b  c  b  0
               b  b  b  b  b
               0  b  0  b  0    16))
      ((= type 12)
        (list  0  0  b  0  0
               0  b  0  b  0
               b  0  c  0  b
               0  b  0  b  0
               0  0  b  0  0    8))
      ((= type (- MAX-TYPE 12))
        (list  b  b  0  b  b
               b  0  b  0  b
               0  b  c  b  0
               b  0  b  0  b
               b  b  0  b  b    16))

      ((= type 13)
        (list  0  b  b  b  0
               b  0  0  0  b
               b  0  c  0  b
               b  0  0  0  b
               0  b  b  b  0    12))
      ((= type (- MAX-TYPE 13))
        (list  b  0  0  0  b
               0  b  b  b  0
               0  b  c  b  0
               0  b  b  b  0
               b  0  0  0  b    12))
      ((= type 14)
        (list  b  0  b  0  b
               0  b  0  b  0
               b  0  c  0  b
               0  b  0  b  0
               b  0  b  0  b    12))
      ((= type (- MAX-TYPE 14))
        (list  0  b  0  b  0
               b  0  b  0  b
               0  b  c  b  0
               b  0  b  0  b
               0  b  0  b  0    12))
      ((= type 15)
        (list  b  b  0  b  b
               b  0  0  0  b
               0  0  c  0  0
               b  0  0  0  b
               b  b  0  b  b    12))
      ((= type (- MAX-TYPE 15))
        (list  0  0  b  0  0
               0  b  b  b  0
               b  b  c  b  b
               0  b  b  b  0
               0  0  b  0  0    12))
      ((= type 16)
        (list  0  b  0  b  0
               b  b  0  b  b
               0  0  c  0  0
               b  b  0  b  b
               0  b  0  b  0    12))
      ((= type (- MAX-TYPE 16))
        (list  b  0  b  0  b
               0  0  b  0  0
               b  b  c  b  b
               0  0  b  0  0
               b  0  b  0  b    12))

      ('else    ; otherwise (error?)
        (list  0  0  0  0  0
               0  0  0  0  0
               0  0  1  0  0
               0  0  0  0  0
               0  0  0  0  0    0))))
))









    ;; Symmetric Blur

(if (not convolution-preset-type-is-basic)
(define (script-fu-convolution-presets-blur-symmetric
            img drawable
            type a c calc_divisor?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((n           (car (last (get-common-matrix-list type 0 0))))
         (b           a)
         (divisor     (if (eqv? calc_divisor? TRUE) (+ (* b n) c) 1))
         (offset      0)
         (matrix-list (get-common-matrix-list type b c))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))
)(if (not convolution-preset-type-is-basic)
(script-fu-register
  "script-fu-convolution-presets-blur-symmetric"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Symmetric Blur..."
  (string-append CONV-PRESET-DESCRIPTION "(Symmetric Blur)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Matrix Type"     CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(3 1 128 1 1 0 0)
  SF-ADJUSTMENT "Center Value"    '(1 1 128 1 1 0 0)
  SF-TOGGLE     "Calculate Divisor" TRUE
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)
)










    ;; Symmetric Edge Detect

(if (not convolution-preset-type-is-basic)
(define (script-fu-convolution-presets-edge-symmetric
            img drawable
            type a mode calc_divisor?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((n           (car (last (get-common-matrix-list type 0 0))))
         (b           a)
         (c           (if (= mode 0) (- 1 (* b n)) (- (* b n))))
         (divisor     1)
         (offset      (cond ((= mode 0)   0)
                            ((= mode 1) 255)
                            ((= mode 2) 128)
                            ((= mode 3)   0)))
         (matrix-list (get-common-matrix-list type b c))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))
)(if (not convolution-preset-type-is-basic)
(script-fu-register
  "script-fu-convolution-presets-edge-symmetric"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Symmetric Edge Detect..."
  (string-append CONV-PRESET-DESCRIPTION "(Symmetric Edge Detect)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Matrix Type"     CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(2 1 128 1 1 0 0)
  SF-OPTION     "Mode"            '("Default" "White" "Gray" "Black")
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)
)










    ;; Symmetric Sharp

(if (not convolution-preset-type-is-basic)
(define (script-fu-convolution-presets-sharp-symmetric
            img drawable
            type a c calc_divisor?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((n           (car (last (get-common-matrix-list type 0 0))))
         (b           a)
         (divisor     (if (eqv? calc_divisor? TRUE) (+ (* b n) c) 1))
         (offset      0)
         (matrix-list (get-common-matrix-list type b c))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))
)(if (not convolution-preset-type-is-basic)
(script-fu-register
  "script-fu-convolution-presets-sharp-symmetric"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Symmetric Sharp..."
  (string-append CONV-PRESET-DESCRIPTION "(Symmetric Sharp)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Matrix Type"     CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(-1 -128 1 1 1 0 1)
  SF-ADJUSTMENT "Center Value"    '( 9  1 128 1 1 0 0)
  SF-TOGGLE     "Calculate Divisor" TRUE
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)
)








    ;; Symmetric General

(if (not convolution-preset-type-is-basic)
(define (script-fu-convolution-presets-general-symmetric
            img drawable
            type a c offset calc_divisor?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((n           (car (last (get-common-matrix-list type 0 0))))
         (b           a)
         (divisor     (if (eqv? calc_divisor? TRUE) (+ (* b n) c) 1))
         (matrix-list (get-common-matrix-list type b c))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))
)(if (not convolution-preset-type-is-basic)
(script-fu-register
  "script-fu-convolution-presets-general-symmetric"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Symmetric General..."
  (string-append CONV-PRESET-DESCRIPTION "(Symmetric General)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Matrix Type"     CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(-2 -128 128  1 1 0 1)
  SF-ADJUSTMENT "Center Value"    '(10 -128 128  1 1 0 1)
  SF-ADJUSTMENT "Offset"          '(0 -9999 9999 1 1 0 1)
  SF-TOGGLE     "Calculate Divisor" TRUE
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)
)








    ;; Sharp


(define (script-fu-convolution-presets-sharp
            img drawable
            type a
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((n           (car (last (get-common-matrix-list type 0 0))))
         (b           (- (/ a (/ n 2))))
         (c           (+ (* a 2) 1))
         (divisor     1)
         (offset      0)
         (matrix-list (get-common-matrix-list type b c))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-sharp"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Sharp..."
  (string-append CONV-PRESET-DESCRIPTION "(Sharp)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Matrix Type"     CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Adjustable Sharp


(define (script-fu-convolution-presets-sharp-adjustable
            img drawable
            type a c
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((n           (car (last (get-common-matrix-list type 0 0))))
         (b           (- (/ a (/ n 2))))
         (divisor     (+ (* b n) c))
         (offset      0)
         (matrix-list (get-common-matrix-list type b c))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-sharp-adjustable"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Adjustable Sharp..."
  (string-append CONV-PRESET-DESCRIPTION "(Adjustable Sharp)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Matrix Type"     CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-ADJUSTMENT "Center Value"    '(10 1 1024 1 1 0 0)
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Unsharp Mask


(define (script-fu-convolution-presets-unsharp-mask
            img drawable
            type a mask-opacity
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

    ;; Return '((layerID visible) ... ) list (bottom to top)
  (define (get-visible-layer-list img)
    (let* ((num_layers (car (gimp-image-get-layers img)))
           (layer_ids (cadr (gimp-image-get-layers img)))
           (layer-list '())
           (index 0))
      (while (< index num_layers)
        (let* ((current-layer (aref layer_ids index))
               (current-visible (car (gimp-drawable-get-visible current-layer))))
          (set! layer-list (cons (list current-layer current-visible)
                                 layer-list))
          (set! index (+ index 1))))
      layer-list))    ; Return

    ;; set car of layer-list visible to status
  (define (set-layer-visible layer-list status)
  )


  (if (equal? (car (gimp-drawable-is-layer drawable)) FALSE)
      (gimp-message "Can not apply this script to Channel or Layer Mask")
      (let* ((old-layer-mode (car (gimp-layer-get-mode drawable)))
             (old-layer-opacity (car (gimp-layer-get-opacity drawable)))
             (dummy (gimp-layer-set-mode drawable NORMAL-MODE))
             (dummy (gimp-layer-set-opacity drawable 100))
             (lighter (car (gimp-layer-copy drawable TRUE)))
             (darker (car (gimp-layer-copy drawable TRUE)))
             (lighter-blured (car (gimp-layer-copy drawable TRUE)))
         (darker-blured)
         (darker-merged)
         (lighter-merged)
         (layer-list-before)
         (layer-list-after)
         (visible-list)
         (invisible-list)
         (final  0)
            )
        (gimp-image-undo-group-start img)
        (if (not (= (car (gimp-layer-get-mask drawable)) -1))
            (gimp-message "Layer mask had been applied into Layer"))
        (gimp-image-add-layer img lighter -1)
        (gimp-image-add-layer img lighter-blured -1)
        (gimp-image-add-layer img darker -1)

        (script-fu-convolution-presets-blur img lighter-blured
                                            type a 0 alpha_alg? b_mode
                                            gray? red? green? blue? alpha?)
        (set! darker-blured (car (gimp-layer-copy lighter-blured TRUE)))
        (gimp-layer-set-mode lighter-blured SUBTRACT-MODE)
        (gimp-layer-set-mode darker SUBTRACT-MODE)
        (set! lighter-merged (car (gimp-image-merge-down img lighter-blured
                                                         EXPAND-AS-NECESSARY)))
        (gimp-image-add-layer img darker-blured -1)
        (set! darker-merged (car (gimp-image-merge-down img darker
                                                        EXPAND-AS-NECESSARY)))
        (gimp-layer-set-opacity darker-merged mask-opacity)
        (gimp-layer-set-opacity lighter-merged mask-opacity)
        (gimp-layer-set-mode lighter-merged ADDITION-MODE)
        (gimp-layer-set-mode darker-merged SUBTRACT-MODE)
    ;; I use gimp-image-merge-visible-layers
    ;; therefore, it's necessary to check other layer's visible
        (set! layer-list-before (get-visible-layer-list img))
        (set! visible-list (mapcar
                (lambda (x)
                  (if (= (cadr x) 1) (car x))) layer-list-before))
        (mapcar (lambda (x) (gimp-drawable-set-visible (car x) FALSE))
                layer-list-before)
        (set-layer-visible layer-list-before FALSE)
        (mapcar (lambda (x) (gimp-drawable-set-visible x TRUE))
                (list lighter-merged darker-merged drawable))
        (gimp-displays-flush)
        (set! final (car (gimp-image-merge-visible-layers img
                                                          EXPAND-AS-NECESSARY)))
    ;; Restore layer's visible
        (set! layer-list-after (get-visible-layer-list img))
        (set! invisible-list (mapcar
                (lambda (x)
                  (if (= (cadr x) 0) (car x))) layer-list-after))
        (let* ((index (- (length visible-list) 1)))
          (while (>= index 0)
            (let* ((current-layer (nth index visible-list)))
              (if (not (eqv? current-layer nil))
                  (if (pair? (member current-layer invisible-list))
                      (gimp-drawable-set-visible current-layer TRUE)))
              (set! index (- index 1)))))
    ;;
        (gimp-layer-set-mode final old-layer-mode)
        (gimp-layer-set-opacity final old-layer-opacity)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush))))

(script-fu-register
  "script-fu-convolution-presets-unsharp-mask"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Unsharp Mask..."
  (string-append CONV-PRESET-DESCRIPTION "(Unsharp Mask)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Type"            CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-ADJUSTMENT "Mask Opacity"    '(50 0 100 1 1 0 1)
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Blur


(define (script-fu-convolution-presets-blur
            img drawable
            type a filter-type
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((n           (car (last (get-common-matrix-list type 0 0))))
         (b           (cond ((= filter-type 0) a)
                            ((= filter-type 1) (/ a (+ n 1))) ))
         (c           (cond ((= filter-type 0) 1)
                            ((= filter-type 1) b) ))
         (divisor     (cond ((= filter-type 0) (+ (* b n) c))
                            ((= filter-type 1) a) ))
         (offset      0)
         (matrix-list (get-common-matrix-list type b c))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-blur"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Blur..."
  (string-append CONV-PRESET-DESCRIPTION "(Blur)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Matrix Type"     CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-OPTION     "Filter Type"     '("Default" "Mean Filter")
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Adjustable blur

(if convolution-preset-type-is-basic        ; if not, use symmetric blur
(define (script-fu-convolution-presets-blur-adjustable
            img drawable
            type a c
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((n           (car (last (get-common-matrix-list type 0 0))))
         (b           a)
         (divisor     (+ (* b n) c))
         (offset      0)
         (matrix-list (get-common-matrix-list type b c))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))
)(if convolution-preset-type-is-basic
(script-fu-register
  "script-fu-convolution-presets-blur-adjustable"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Adjustable Blur..."
  (string-append CONV-PRESET-DESCRIPTION "(Adjustable Blur)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Matrix Type"     CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(10 1 1024 1 1 0 0)
  SF-ADJUSTMENT "Center Value"    '(10 1  256 1 1 0 0)
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)
)








    ;; Blur Gaussian


(define (script-fu-convolution-presets-blur-gaussian
            img drawable
            type a filter-type radius
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
  (define vert  0)
  (define std_dev  0)
  (define div  0)
  (define div_pi  0)
  (set! vert (+ (abs radius) 1.0))
  ;; std_dev = sqrt (-(vert * vert) / (2 * log (1.0 / 255.0)))
  (set! std_dev (sqrt (/ (- (* vert vert)) (* 2 (log (/ 1 255))))))
  (set! div (* 2 (pow std_dev 2)))
  (set! div_pi (* div *pi*))

  ;; G(x y) = -1 / ((sqrt 2 PI) * std_dev) *
  ;;          e ^ ( -(x^2 + y^2) / 2 * std_div^2)
  (define (get-gaussian x y)
    (* (/ 1 div_pi)
       (pow (exp 1) (- (/ (+ (pow x 2) (pow y 2)) div)))))

  ;; delta^2 G(x y) = -1 / (2 PI * std_div^4) *
  ;;                  (2 - (x^2 + y^2) / std_div^2) *
  ;;                  e ^ ( -((x^2 + y^2) / (2 * std_div^2))
  (define (get-LoG x y)
    (* (/ -1 (* 2 *pi* (pow std_dev 4)))
       (- 2 (/ (+ (pow x 2) (pow y 2)) (pow std_dev 2)))
       (pow (exp 1) (- (/ (+ (pow x 2) (pow y 2)) div)))))

  (define (get-gauss-distribution p)
    (cond
      ((= filter-type 0)
        (* a (cond
              ((= p 1) (get-gaussian 2 2))
              ((= p 2) (get-gaussian 2 1))
              ((= p 3) (get-gaussian 2 0))
              ((= p 4) (get-gaussian 1 1))
              ((= p 5) (get-gaussian 1 0))
              ((= p 6) (get-gaussian 0 0))
      )))
      ((= filter-type 1)
        (* a (cond
              ((= p 1) (get-LoG 2 2))
              ((= p 2) (get-LoG 2 1))
              ((= p 3) (get-LoG 2 0))
              ((= p 4) (get-LoG 1 1))
              ((= p 5) (get-LoG 1 0))
              ((= p 6) (get-LoG 0 0))
      )))
      ((= filter-type 2)
        (+ a p))
      ((= filter-type 3)
        (- a p))
      ((= filter-type 4)
        (* a p))
      ((= filter-type 5)
        (/ a p))
      ((= filter-type 6)
        (/ p a))
      ((= filter-type 7)
        (pow a (/ 1 p)))
      ((= filter-type 8)
        (pow a p))
  ))
  (define ggd get-gauss-distribution)

  (let* ((n           (car (last (get-common-matrix-list type 0 0))))
         (offset      0)
         (matrix-list1 (butlast (get-common-matrix-list type 1 1)))
         (matrix-list2 (list (ggd 1) (ggd 2) (ggd 3) (ggd 2) (ggd 1)
                             (ggd 2) (ggd 4) (ggd 5) (ggd 4) (ggd 2)
                             (ggd 3) (ggd 5) (ggd 6) (ggd 5) (ggd 3)
                             (ggd 2) (ggd 4) (ggd 5) (ggd 4) (ggd 2)
                             (ggd 1) (ggd 2) (ggd 3) (ggd 2) (ggd 1) ))
         (matrix-list (mapcar * matrix-list1 matrix-list2))
         (divisor     (apply + matrix-list))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

;(conv-preset-debug-output matrix channels alpha_alg bmode)

    (gimp-image-undo-group-start img)
    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-blur-gaussian"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Blur (Gaussian)..."
  (string-append CONV-PRESET-DESCRIPTION "(Blur Gaussian)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Type"            CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-OPTION     "Filter Type"     '("Gaussian" "LoG (Edge detect)"
                                    "Add" "Subst" "Multiply" "Divide" "InDvide"
                                    "Root" "Power" )
  SF-ADJUSTMENT "Radius"         '(1.0 0.1 16.0 1 1 1 0)
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Edge detect


(define (script-fu-convolution-presets-edge-detect
            img drawable
            type a mode
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((n           (car (last (get-common-matrix-list type 0 0))))
         (b           (/ (+ a 1) n))
         (c           (if (= mode 0) (- a) (+ -1 (- a))))
         (divisor     1)
         (offset      (cond ((= mode 0) 0)
                            ((= mode 1) 255)
                            ((= mode 2) 128)
                            ((= mode 3)   0)))
         (matrix-list (get-common-matrix-list type b c))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-edge-detect"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Edge Detect..."
  (string-append CONV-PRESET-DESCRIPTION "(Edge Detect)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Matrix Type"     CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-OPTION     "Mode"            '("Default" "White" "Gray" "Black")
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Edge Detect (Laplace)


(define (script-fu-convolution-presets-edge-detect-laplace
            img drawable
            type a color-mode gauss? invert? desaturate?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((n           (car (last (get-common-matrix-list type 0 0))))
         (b           a)
         (c           (* (- a) n))
         (divisor     (if (eqv? invert? TRUE) -1 1))
         (offset      (cond ((= color-mode 0) 255)
                            ((= color-mode 1) 128)
                            ((= color-mode 2)   0)
                            ((= color-mode 3)   0)))
         (matrix-list (get-common-matrix-list type b c))
         (matrix (get-matrix matrix-list))
         (channels (get-channels drawable gray? red? green? blue? alpha?))
         (listed (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg (car listed))
         (bmode (cadr listed)))

    (gimp-image-undo-group-start img)
    (if (and (eqv? desaturate? TRUE) (not (= color-mode 3))
             (eqv? (car (gimp-image-base-type img)) RGB))
        (gimp-desaturate drawable))
    (if (eqv? gauss? TRUE)
        (script-fu-convolution-presets-blur-gaussian
            img drawable
            type a 0 1
            alpha_alg? b_mode
            gray? red? green? blue? alpha?))
    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-edge-detect-laplace"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Edge Detect (Laplace)..."
  (string-append CONV-PRESET-DESCRIPTION "(Edge Detect Laplace)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Type"            CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-OPTION     "Color Mode"      '("White" "Gray" "Black" "Colored")
  SF-TOGGLE     "Pre-appling Gauss Blur" FALSE
  SF-TOGGLE     "Invert"          FALSE
  SF-TOGGLE     "Desaturate"      TRUE
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Edge Detect (Sobel)


(define (script-fu-convolution-presets-edge-detect-sobel
            img drawable
            layer-mode a filter-type invert? desaturate?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (if (equal? (car (gimp-drawable-is-layer drawable)) FALSE)
      (gimp-message "Can not apply this script to Channel or Layer Mask")
      (let* ((old-layer-mode (car (gimp-layer-get-mode drawable)))
             (old-layer-opacity (car (gimp-layer-get-opacity drawable)))
             (dummy (gimp-layer-set-mode drawable NORMAL-MODE))
             (dummy (gimp-layer-set-opacity drawable 100))
             (layer (car (gimp-layer-copy drawable TRUE)))
             (-a          (- a))
             ( 2a         (* 2 a))
             (-2a         (- 2a))
             (divisor     (if (eqv? invert? TRUE) -1 1))
             (offset      (cond ((= layer-mode 0) 0)
                                ((= layer-mode 1) 128)
                                ((= layer-mode 2) 255)))
             (channels (get-channels drawable gray? red? green? blue? alpha?))
             (listed (get-alpha_alg-bmode drawable alpha_alg? b_mode))
             (alpha_alg (car listed))
             (bmode (cadr listed))
          (matrix  0)
           (matrix-list  0)
         (final  0)
            )

        (gimp-image-undo-group-start img)
        (gimp-image-add-layer img layer -1)
        (if (not (= (car (gimp-layer-get-mask drawable)) -1))
            (gimp-message "Layer mask had been applied into Layer"))
        (if (and (eqv? desaturate? TRUE)
                 (eqv? (car (gimp-image-base-type img)) RGB))
            (begin
              (gimp-desaturate drawable)
              (gimp-desaturate layer)))


    ;; Horizontal
        (set! matrix-list (cond
          ((= filter-type 0)
            (list 0   0   0   0   0
                  0  -a   0   a   0
                  0 -2a   0  2a   0
                  0  -a   0   a   0
                  0   0   0   0   0))
          ((= filter-type 1)
            (list 0   0   0   0   0
                  0  -a   0   a   0
                  0  -a   0   a   0
                  0  -a   0   a   0
                  0   0   0   0   0))
          ((= filter-type 2)
            (list 0   0   0   0   0
                  0   0   0   0   0
                  0   0   a  -a   0
                  0   0   0   0   0
                  0   0   0   0   0))
          ((= filter-type 3)
            (list 0   0   0   0   0
                  0   0   0   0   0
                  0   0   a   0   0
                  0   0   0  -a   0
                  0   0   0   0   0))
          ((= filter-type 4)
            (list 0   0   0   0   0
                  0   0   0   0   0
                  0   0  -a   a   0
                  0   0  -a   a   0
                  0   0   0   0   0))
        ))

        (set! matrix (get-matrix matrix-list))
        (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                            divisor offset argc_channles channels bmode)

    ;; Vertical
        (set! matrix-list (cond
          ((= filter-type 0)
            (list 0   0   0   0   0
                  0  -a -2a  -a   0
                  0   0   0   0   0
                  0   a  2a   a   0
                  0   0   0   0   0))
          ((= filter-type 1)
            (list 0   0   0   0   0
                  0  -a  -a  -a   0
                  0   0   0   0   0
                  0   a   a   a   0
                  0   0   0   0   0))
          ((= filter-type 2)
            (list 0   0   0   0   0
                  0   0   0   0   0
                  0   0   a   0   0
                  0   0  -a   0   0
                  0   0   0   0   0))
          ((= filter-type 3)
            (list 0   0   0   0   0
                  0   0   0   0   0
                  0   0   0   a   0
                  0   0  -a   0   0
                  0   0   0   0   0))
          ((= filter-type 4)
            (list 0   0   0   0   0
                  0   0   0   0   0
                  0   0  -a  -a   0
                  0   0   a   a   0
                  0   0   0   0   0))
        ))
        (set! matrix (get-matrix matrix-list))
        (plug-in-convmatrix 1 img layer argc_matrix matrix alpha_alg
                            divisor offset argc_channles channels bmode)

        (if (or (= layer-mode 0) (= layer-mode 1))
            (gimp-layer-set-mode layer ADDITION-MODE)
            (gimp-layer-set-mode layer MULTIPLY-MODE))

        (set! final (car (gimp-image-merge-down img layer EXPAND-AS-NECESSARY)))
        (gimp-layer-set-mode final old-layer-mode)
        (gimp-layer-set-opacity final old-layer-opacity)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush))))

(script-fu-register
  "script-fu-convolution-presets-edge-detect-sobel"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Edge Detect (Sobel)..."
  (string-append CONV-PRESET-DESCRIPTION "(Edge Detect Sobel)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  ;SF-OPTION     "Type"            CONV-PRESET-TYPE-LIST
  SF-OPTION     "Layer Mode"      '("Addition (Black)" "Addition (White)"
                                    "Mutiply")
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-OPTION     "Filter Type"     '("Sobel Fiter" "Prewitt Filter" "Gradient"
                                    "Roberts Filter" "Liner Difference")
  SF-TOGGLE     "Invert"          FALSE
  SF-TOGGLE     "Desaturate"      TRUE
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Edge Detect (Prewitt)
    ;;   (template matting)


(define (script-fu-convolution-presets-edge-detect-prewitt
            img drawable
            a color-mode invert? desaturate?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (if (equal? (car (gimp-drawable-is-layer drawable)) FALSE)
      (gimp-message "Can not apply this script to Channel or Layer Mask")
      (let* ((old-layer-mode (car (gimp-layer-get-mode drawable)))
             (old-layer-opacity (car (gimp-layer-get-opacity drawable)))
             (dummy (gimp-layer-set-mode drawable NORMAL-MODE))
             (dummy (gimp-layer-set-opacity drawable 100))
             (layer (car (gimp-layer-copy drawable TRUE)))
             (-a          (- a))
             ( 2a         (* 2 a))
             (-2a         (- 2a))
             (divisor     (if (eqv? invert? TRUE) -1 1))
             (offset      (cond ((= color-mode 0) 255)
                                ((= color-mode 1) 128)
                                ((= color-mode 2)   0)))
             (channels (get-channels drawable gray? red? green? blue? alpha?))
             (listed (get-alpha_alg-bmode drawable alpha_alg? b_mode))
             (alpha_alg (car listed))
             (bmode (cadr listed))
             (matrix  0)
             (work-layer  0)
             (layer-copy  0)
             (count  0)
         (final  0)
            )

    ;; get mask pattern
        (define (get-prewitt-mask-pattern mask-pattern)
          (cond
          ((= mask-pattern 0)
            (list 0   0   0   0   0
                  0   a   a   a   0
                  0   a -2a   a   0
                  0  -a  -a  -a   0
                  0   0   0   0   0))
          ((= mask-pattern 1)
            (list 0   0   0   0   0
                  0   a   a   a   0
                  0   a -2a  -a   0
                  0   a  -a  -a   0
                  0   0   0   0   0))
          ((= mask-pattern 2)
            (list 0   0   0   0   0
                  0   a   a  -a   0
                  0   a -2a  -a   0
                  0   a   a  -a   0
                  0   0   0   0   0))
          ((= mask-pattern 3)
            (list 0   0   0   0   0
                  0   a  -a  -a   0
                  0   a -2a  -a   0
                  0   a   a   a   0
                  0   0   0   0   0))
          ((= mask-pattern 4)
            (list 0   0   0   0   0
                  0  -a  -a  -a   0
                  0   a -2a   a   0
                  0   a   a   a   0
                  0   0   0   0   0))
          ((= mask-pattern 5)
            (list 0   0   0   0   0
                  0  -a  -a   a   0
                  0  -a -2a   a   0
                  0   a   a   a   0
                  0   0   0   0   0))
          ((= mask-pattern 6)
            (list 0   0   0   0   0
                  0  -a   a   a   0
                  0  -a -2a   a   0
                  0  -a   a   a   0
                  0   0   0   0   0))
          ((= mask-pattern 7)
            (list 0   0   0   0   0
                  0   a   a   a   0
                  0  -a -2a   a   0
                  0  -a  -a   a   0
                  0   0   0   0   0))
        ))

        (gimp-image-undo-group-start img)
        (gimp-image-add-layer img layer -1)
        (if (not (= (car (gimp-layer-get-mask drawable)) -1))
            (gimp-message "Layer mask had been applied into Layer"))
        (if (and (eqv? desaturate? TRUE)
                 (eqv? (car (gimp-image-base-type img)) RGB))
            (begin
              (gimp-desaturate drawable)
              (gimp-desaturate layer)))

        (set! matrix (get-matrix (get-prewitt-mask-pattern 0)))
        (plug-in-convmatrix 1 img layer argc_matrix matrix alpha_alg
                            divisor offset argc_channles channels bmode)

        (set! count 1)
        (while (< count 8)
          (set! layer-copy (car (gimp-layer-copy drawable TRUE)))
          (gimp-image-add-layer img layer-copy -1)
          (gimp-layer-set-mode layer-copy (cond
                                   ((= color-mode 0) DARKEN-ONLY-MODE)
                                   ((= color-mode 1) OVERLAY-MODE)
                                   ((= color-mode 2) LIGHTEN-ONLY-MODE)))
          (set! matrix (get-matrix (get-prewitt-mask-pattern count)))
          (plug-in-convmatrix 1 img layer-copy argc_matrix matrix alpha_alg
                              divisor offset argc_channles channels bmode)
          (set! work-layer (car (gimp-image-merge-down img layer-copy
                                                       EXPAND-AS-NECESSARY)))
          (gimp-layer-set-mode work-layer NORMAL-MODE)
          (set! count (+ count 1)))

        (set! final (car (gimp-image-merge-down img work-layer
                                                EXPAND-AS-NECESSARY)))
        (gimp-layer-set-mode final old-layer-mode)
        (gimp-layer-set-opacity final old-layer-opacity)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush))))

(script-fu-register
  "script-fu-convolution-presets-edge-detect-prewitt"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Edge Detect (Prewitt)..."
  (string-append CONV-PRESET-DESCRIPTION "(Edge Detect Prewitt)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  ;SF-OPTION     "Type"            CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-OPTION     "Color Mode"      '("White" "Gray" "Black")
  SF-TOGGLE     "Invert"          FALSE
  SF-TOGGLE     "Desaturate"      TRUE
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Edge Detect (Line Detection)


(define (script-fu-convolution-presets-edge-detect-line
            img drawable
            a angle color-mode desaturate?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((-a          (- a))
         ( 2a         (* 2 a))
         (-2a         (- 2a))
         (c           (if (= color-mode 3) (+ 2a 1) 2a))
         (divisor     1)
         (offset      (cond ((= color-mode 0) 255)
                            ((= color-mode 1) 128)
                            ((= color-mode 2)   0)
                            ((= color-mode 3)   0)))
         (channels (get-channels drawable gray? red? green? blue? alpha?))
         (listed (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg (car listed))
         (bmode (cadr listed))
         (matrix-list  0)
         (matrix  0)
        )

    (gimp-image-undo-group-start img)
    (if (and (and (eqv? desaturate? TRUE) (not (= color-mode 3)))
             (and (not (= angle 4)) (not (= angle 5)))
             (eqv? (car (gimp-image-base-type img)) RGB))
        (gimp-desaturate drawable))

    (set! matrix-list (cond
      ((= angle 0)
        (list 0   0   0   0   0
              0  -a  -a  -a   0
              0  2a   c  2a   0
              0  -a  -a  -a   0
              0   0   0   0   0))
      ((= angle 1)
        (list 0   0   0   0   0
              0  -a  2a  -a   0
              0  -a   c  -a   0
              0  -a  2a  -a   0
              0   0   0   0   0))
      ((= angle 2)
        (list 0   0   0   0   0
              0  -a  -a  2a   0
              0  -a   c  -a   0
              0  2a  -a  -a   0
              0   0   0   0   0))
      ((= angle 3)
        (list 0   0   0   0   0
              0  2a  -a  -a   0
              0  -a   c  -a   0
              0  -a  -a  2a   0
              0   0   0   0   0))
    ))

    (cond
      ((or (= angle 0) (= angle 1) (= angle 2) (= angle 3))
        (set! matrix (get-matrix matrix-list))
        (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                            divisor offset argc_channles channels bmode))
      ((= angle 4)
        (if (equal? (car (gimp-drawable-is-layer drawable)) FALSE)
            (gimp-message "Can not apply this script to Channel or Layer Mask")
            (let* ((old-layer-mode (car (gimp-layer-get-mode drawable)))
                   (old-layer-opacity (car (gimp-layer-get-opacity drawable)))
                   (dummy (gimp-layer-set-mode drawable NORMAL-MODE))
                   (dummy (gimp-layer-set-opacity drawable 100))
                   (layer1 (car (gimp-layer-copy drawable TRUE)))
                   (layer2 (car (gimp-layer-copy drawable TRUE))))
              (gimp-image-add-layer img layer1 -1)
              (gimp-image-add-layer img layer2 -1)
              (gimp-layer-set-mode layer1 NORMAL-MODE)
              (cond ((= color-mode 0)
                       (gimp-layer-set-mode layer2 MULTIPLY-MODE))
                    ((= color-mode 1)
                       (gimp-layer-set-mode layer2 LIGHTEN-ONLY-MODE))
                    ((= color-mode 2)
                       (gimp-layer-set-mode layer2 ADDITION-MODE))
                    ((= color-mode 3)
                       (gimp-layer-set-mode layer2 LIGHTEN-ONLY-MODE)))
              (script-fu-convolution-presets-edge-detect-line
            img layer1
            a 0 color-mode desaturate?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
              (script-fu-convolution-presets-edge-detect-line
            img layer2
            a 1 color-mode desaturate?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
             (if (not (= (car (gimp-layer-get-mask drawable)) -1))
                 (gimp-message "Layer mask had been applied into Layer"))
              (set! drawable (car
               (gimp-image-merge-down img (car
                (gimp-image-merge-down img layer2 CLIP-TO-BOTTOM-LAYER))
                                      CLIP-TO-BOTTOM-LAYER)))
              (gimp-layer-set-mode drawable old-layer-mode)
              (gimp-layer-set-opacity drawable old-layer-opacity) )))
      ((= angle 5)
        (if (equal? (car (gimp-drawable-is-layer drawable)) FALSE)
            (gimp-message "Can not apply this script to Channel or Layer Mask")
            (let* ((old-layer-mode (car (gimp-layer-get-mode drawable)))
                   (old-layer-opacity (car (gimp-layer-get-opacity drawable)))
                   (dummy (gimp-layer-set-mode drawable NORMAL-MODE))
                   (dummy (gimp-layer-set-opacity drawable 100))
                   (layer1 (car (gimp-layer-copy drawable TRUE)))
                   (layer2 (car (gimp-layer-copy drawable TRUE))))
              (gimp-image-add-layer img layer1 -1)
              (gimp-image-add-layer img layer2 -1)
              (gimp-layer-set-mode layer1 NORMAL-MODE)
              (cond ((= color-mode 0)
                       (gimp-layer-set-mode layer2 MULTIPLY-MODE))
                    ((= color-mode 1)
                       (gimp-layer-set-mode layer2 LIGHTEN-ONLY-MODE))
                    ((= color-mode 2)
                       (gimp-layer-set-mode layer2 ADDITION-MODE))
                    ((= color-mode 3)
                       (gimp-layer-set-mode layer2 LIGHTEN-ONLY-MODE)))
              (script-fu-convolution-presets-edge-detect-line
            img layer1
            a 2 color-mode desaturate?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
              (script-fu-convolution-presets-edge-detect-line
            img layer2
            a 3 color-mode desaturate?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
              (if (not (= (car (gimp-layer-get-mask drawable)) -1))
                  (gimp-message "Layer mask had been applied into Layer"))
              (set! drawable (car
               (gimp-image-merge-down img (car
                (gimp-image-merge-down img layer2 CLIP-TO-BOTTOM-LAYER))
                                      CLIP-TO-BOTTOM-LAYER)))
              (gimp-layer-set-mode drawable old-layer-mode)
              (gimp-layer-set-opacity drawable old-layer-opacity) )))
    ) ; end of cond
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
))

(script-fu-register
  "script-fu-convolution-presets-edge-detect-line"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Edge Detect (Line)..."
  (string-append CONV-PRESET-DESCRIPTION "(Edge Detect Line Detection)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-OPTION     "Angle"           '("Horizontal" "Vertical" "+45" "-45"
                                    "Cross (+)" "Cross (x)")
  SF-OPTION     "Color Mode"      '("White" "Gray" "Black" "Colored")
  SF-TOGGLE     "Desaturate"      TRUE
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Edge Detect (Compass)


(define (script-fu-convolution-presets-edge-detect-compass
            img drawable
            a angle color-mode desaturate?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((-a          (- a))
         ( 2a         (* 2 a))
         (-2a         (- 2a))
         (c           (if (= color-mode 3) (+ -2a 1) -2a))
         (divisor     1)
         (offset      (cond ((= color-mode 0) 255)
                            ((= color-mode 1) 128)
                            ((= color-mode 2)   0)
                            ((= color-mode 3)   0)))
         (channels (get-channels drawable gray? red? green? blue? alpha?))
         (listed (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg (car listed))
         (bmode (cadr listed))
      (matrix-list  0)
         (matrix  0))

    (gimp-image-undo-group-start img)
    (if (and (and (eqv? desaturate? TRUE) (not (= color-mode 3)))
             (eqv? (car (gimp-image-base-type img)) RGB))
        (gimp-desaturate drawable))
    (set! matrix-list (cond
      ((= angle 0)
        (list 0   0   0   0   0
              0  -a   a   a   0
              0  -a   c   a   0
              0  -a   a   a   0
              0   0   0   0   0))
      ((= angle 1)
        (list 0   0   0   0   0
              0   a   a   a   0
              0  -a   c   a   0
              0  -a  -a   a   0
              0   0   0   0   0))
      ((= angle 2)
        (list 0   0   0   0   0
              0   a   a   a   0
              0   a   c   a   0
              0  -a  -a  -a   0
              0   0   0   0   0))
      ((= angle 3)
        (list 0   0   0   0   0
              0   a   a   a   0
              0   a   c  -a   0
              0   a  -a  -a   0
              0   0   0   0   0))
      ((= angle 4)
        (list 0   0   0   0   0
              0   a   a  -a   0
              0   a   c  -a   0
              0   a   a  -a   0
              0   0   0   0   0))
      ((= angle 5)
        (list 0   0   0   0   0
              0   a  -a  -a   0
              0   a   c  -a   0
              0   a   a   a   0
              0   0   0   0   0))
      ((= angle 6)
        (list 0   0   0   0   0
              0  -a  -a  -a   0
              0   a   c   a   0
              0   a   a   a   0
              0   0   0   0   0))
      ((= angle 7)
        (list 0   0   0   0   0
              0  -a  -a   a   0
              0  -a   c   a   0
              0   a   a   a   0
              0   0   0   0   0))
    ))

    (set! matrix (get-matrix matrix-list))
    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)

    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
))

(script-fu-register
  "script-fu-convolution-presets-edge-detect-compass"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Edge Detect (Compass)..."
  (string-append CONV-PRESET-DESCRIPTION "(Edge Detect Compass)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-OPTION     "Angle"           '("  0" " 45" " 90" "135"
                                    "180" "215" "270" "315")
  SF-OPTION     "Color Mode"      '("White" "Gray" "Black" "Colored")
  SF-TOGGLE     "Desaturate"      TRUE
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Edge Detect (using blur)


(define (script-fu-convolution-presets-edge-detect-blur
            img drawable
            type a detect-type blur-type desaturate?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (if (equal? (car (gimp-drawable-is-layer drawable)) FALSE)
      (gimp-message "Can not apply this script to Channel or Layer Mask")
      (let* ((old-layer-mode (car (gimp-layer-get-mode drawable)))
             (old-layer-opacity (car (gimp-layer-get-opacity drawable)))
             (dummy (gimp-layer-set-mode drawable NORMAL-MODE))
             (dummy (gimp-layer-set-opacity drawable 100))
             (layer (car (gimp-layer-copy drawable TRUE)))
             (final  0)
            )
        (gimp-image-undo-group-start img)
        (gimp-image-add-layer img layer -1)
        (if (and (eqv? desaturate? TRUE)
                 (eqv? (car (gimp-image-base-type img)) RGB))
            (begin
              (gimp-desaturate drawable)
              (gimp-desaturate layer)))
        (script-fu-convolution-presets-blur img layer type a blur-type
                                            alpha_alg? b_mode
                                            gray? red? green? blue? alpha?)
        (cond ((= detect-type 0)
                 (gimp-layer-set-mode layer DIVIDE-MODE))
              ((= detect-type 1)
                 (gimp-layer-set-opacity layer 50)
                 (gimp-invert layer)))
        (if (not (= (car (gimp-layer-get-mask layer)) -1))
            (gimp-message "Layer mask had been applied into Layer"))

        (set! final (car (gimp-image-merge-down img layer EXPAND-AS-NECESSARY)))
        (gimp-layer-set-mode final old-layer-mode)
        (gimp-layer-set-opacity final old-layer-opacity)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush))))

(script-fu-register
  "script-fu-convolution-presets-edge-detect-blur"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Edge Detect (Blur)..."
  (string-append CONV-PRESET-DESCRIPTION "(Edge Detect using Blur)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Type"            CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-OPTION     "Detect Type"     '("Divide" "Invert")
  SF-OPTION     "Blur Filter"     '("Default" "Mean Filter")
  SF-TOGGLE     "Desaturate"      TRUE
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Blur edge


(define (script-fu-convolution-presets-blur-edge
            img drawable
            type a
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((n           (car (last (get-common-matrix-list type 0 0))))
         (b           (/ (+ a n) n))
         (c           (- a))
         (divisor     (+ (* b n) c))
         (offset      0)
         (matrix-list (get-common-matrix-list type b c))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-blur-edge"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Blur Edge..."
  (string-append CONV-PRESET-DESCRIPTION "(Blur Edge)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Matrix Type"     CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Vibration


(define (script-fu-convolution-presets-vibration
            img drawable
            type a motion-blur?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (define divisor  0)
  (define offset  0)
  (define matrix-list  0)
  (define n  0)
  (define c  0)
  (set! n (if (and (<= 12 type) (<= type 15)) 4 2))
  (set! c (if (eqv? motion-blur? TRUE) a 0))
  (set! divisor (+ (* a n) c))
  (set! offset 0)
  (set! matrix-list (cond
    ((= type 0)
      (list   0   0   0   0   0
              0   0   0   0   0
              0   a   c   a   0
              0   0   0   0   0
              0   0   0   0   0))
    ((= type 1)
      (list   0   0   0   0   0
              0   a   0   0   0
              0   0   c   0   0
              0   0   0   a   0
              0   0   0   0   0))
    ((= type 2)
      (list   0   0   0   0   0
              0   0   a   0   0
              0   0   c   0   0
              0   0   a   0   0
              0   0   0   0   0))
    ((= type 3)
      (list   0   0   0   0   0
              0   0   0   a   0
              0   0   c   0   0
              0   a   0   0   0
              0   0   0   0   0))
    ((= type 4)
      (list   0   0   0   0   0
              0   0   0   0   0
              a   0   c   0   a
              0   0   0   0   0
              0   0   0   0   0))
    ((= type 5)
      (list   0   0   0   0   0
              a   0   0   0   0
              0   0   c   0   0
              0   0   0   0   a
              0   0   0   0   0))
    ((= type 6)
      (list   a   0   0   0   0
              0   0   0   0   0
              0   0   c   0   0
              0   0   0   0   0
              0   0   0   0   a))
    ((= type 7)
      (list   0   a   0   0   0
              0   0   0   0   0
              0   0   c   0   0
              0   0   0   0   0
              0   0   0   a   0))
    ((= type 8)
      (list   0   0   a   0   0
              0   0   0   0   0
              0   0   c   0   0
              0   0   0   0   0
              0   0   a   0   0))
    ((= type 9)
      (list   0   0   0   a   0
              0   0   0   0   0
              0   0   c   0   0
              0   0   0   0   0
              0   a   0   0   0))
    ((= type 10)
      (list   0   0   0   0   a
              0   0   0   0   0
              0   0   c   0   0
              0   0   0   0   0
              a   0   0   0   0))
    ((= type 11)
      (list   0   0   0   0   0
              0   0   0   0   a
              0   0   c   0   0
              a   0   0   0   0
              0   0   0   0   0))
    ((= type 12)
      (list   0   0   0   0   0
              0   0   0   0   0
              a   a   c   a   a
              0   0   0   0   0
              0   0   0   0   0))
    ((= type 13)
      (list   a   0   0   0   0
              0   a   0   0   0
              0   0   c   0   0
              0   0   0   a   0
              0   0   0   0   a))
    ((= type 14)
      (list   0   0   a   0   0
              0   0   a   0   0
              0   0   c   0   0
              0   0   a   0   0
              0   0   a   0   0))
    ((= type 15)
      (list   0   0   0   0   a
              0   0   0   a   0
              0   0   c   0   0
              0   a   0   0   0
              a   0   0   0   0))
   )) ; end of cond

  (let* ((matrix (get-matrix matrix-list))
         (channels (get-channels drawable gray? red? green? blue? alpha?))
         (listed (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg (car listed))
         (bmode (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-vibration"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Vibration (Motion Blur)..."
  (string-append CONV-PRESET-DESCRIPTION "(Vibration)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Angle"           '( "0 (Small)"  "45 (Small)"
                                    "90 (Small)" "135 (Small)"
                                     "0"   "30"   "45"   "60"
                                    "90"  "120"  "135"  "150"
                                     "0 (Wide)"   "45 (Wide)"
                                    "90 (Wide)"  "135 (Wide)")
  SF-ADJUSTMENT "Amount"          '(3.0 0.1 16.0 1 1 1 0)
  SF-TOGGLE     "Motion Blur"     FALSE
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Emboss Standard


(define (script-fu-convolution-presets-emboss-standard
            img drawable
            desaturate-type type a strong? flip?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (define divisor  0)
  (define a  0)
  (define -a  0)
  (define b  0)
  (define -b  0)
  (define offset  0)
  (define c  0)
  (define matrix-list  0)
  (set! a (if (eqv? flip? TRUE) (- a) a))
  (set! -a (- a))
  (set! b (if (eqv? strong? TRUE) (/ a 2) 0))
  (set! -b (if (eqv? strong? TRUE) (/ -a 2) 0))
  (set! offset (if (or (= desaturate-type 1) (= desaturate-type 3)) 128 0))
  (set! c      (if (or (= desaturate-type 1) (= desaturate-type 3))   0 1))
  (set! divisor 1)
  (set! matrix-list (cond
    ((= type 0)
      (list   0   0   0   0   0
              0   b   0  -b   0
              0   a   c  -a   0
              0   b   0  -b   0
              0   0   0   0   0))
    ((= type 1)
      (list   0   0   0   0   0
              0   a   b   0   0
              0   b   c  -b   0
              0   0  -b  -a   0
              0   0   0   0   0))
    ((= type 2)
      (list   0   0   0   0   0
              0   b   a   b   0
              0   0   c   0   0
              0  -b  -a  -b   0
              0   0   0   0   0))
    ((= type 3)
      (list   0   0   0   0   0
              0   0   b   a   0
              0  -b   c   b   0
              0  -a  -b   0   0
              0   0   0   0   0))
    ((= type 4)
      (list   0   0   0   0   0
              b   0   0   0  -b
              a   0   c   0  -a
              b   0   0   0  -b
              0   0   0   0   0))
    ((= type 5)
      (list   b   0   0   0   0
              a   0   0   0   0
              b   0   c   0  -b
              0   0   0   0  -a
              0   0   0   0  -b))
    ((= type 6)
      (list   a   b   0   0   0
              b   0   0   0   0
              0   0   c   0   0
              0   0   0   0  -b
              0   0   0  -b  -a))
    ((= type 7)
      (list   b   a   b   0   0
              0   0   0   0   0
              0   0   c   0   0
              0   0   0   0   0
              0   0  -b  -a  -b))
    ((= type 8)
      (list   0   b   a   b   0
              0   0   0   0   0
              0   0   c   0   0
              0   0   0   0   0
              0  -b  -a  -b   0))
    ((= type 9)
      (list   0   0   b   a   b
              0   0   0   0   0
              0   0   c   0   0
              0   0   0   0   0
             -b  -a  -b   0   0))
    ((= type 10)
      (list   0   0   0   b   a
              0   0   0   0   b
              0   0   c   0   0
             -b   0   0   0   0
             -a  -b   0   0   0))
    ((= type 11)
      (list   0   0   0   0   b
              0   0   0   0   a
             -b   0   c   0   b
             -a   0   0   0   0
             -b   0   0   0   0))
    ((= type 12)
      (list   b   0   0   0  -b
              b   b   0  -b  -b
              a   a   c  -a  -a
              b   b   0  -b  -b
              b   0   0   0  -b))
    ((= type 13)
      (list   a   b   b   0   0
              b   a   b   0   0
              b   b   c  -b  -b
              0   0  -b  -a  -b
              0   0  -b  -b  -a))
    ((= type 14)
      (list   b   b   a   b   b
              0   b   a   b   0
              0   0   c   0   0
              0  -b  -a  -b   0
             -b  -b  -a  -b  -b))
    ((= type 15)
      (list   0   0   b   b   a
              0   0   b   a   b
             -b  -b   c   b   b
             -b  -a  -b   0   0
             -a  -b  -b   0   0))
   )) ; end of cond

  (let* ((matrix (get-matrix matrix-list))
         (channels (get-channels drawable gray? red? green? blue? alpha?))
         (listed (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg (car listed))
         (bmode (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (if (and (or (= desaturate-type 2) (= desaturate-type 3))
             (eqv? (car (gimp-image-base-type img)) RGB))
        (gimp-desaturate drawable))
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-emboss-standard"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Emboss Standard..."
  (string-append CONV-PRESET-DESCRIPTION "(Emboss Standard)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Color"           '("Colored Emboss" "Color Edge"
                                    "White-Black Emboss" "Gray Emboss")
  SF-OPTION     "Angle"           '("  0 (Small)"  " 45 (Small)"
                                    " 90 (Small)"  "135 (Small)"
                                    "  0"   " 30"  " 45"   " 60"
                                    " 90"   "120"  "135"   "150"
                                    "  0 (Strong)" " 45 (Strong)"
                                    " 90 (Strong)" "135 (Strong)")
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-TOGGLE     "More Strong"     FALSE
  SF-TOGGLE     "Angle Flip"      FALSE
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Emboss


(define (script-fu-convolution-presets-emboss
            img drawable
            type desaturate-type angle a
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  ;(define (sine degree)
  ;  (* a (sin (/ (* 2 (+ angle degree) *pi*) 360))))

  (define (sine degree)
    (* a (*      (sin (/ (* 2 (+ angle degree) *pi*) 360))
            (abs (sin (/ (* 2 (+ angle degree) *pi*) 360))) )))

  ;(define (sine degree)
  ;  (* a (*      (sin (/ (* 2 (+ angle degree) *pi*) 360))
  ;          (abs (sin (/ (* 2 (+ angle degree) *pi*) 360)))
  ;          (abs (sin (/ (* 2 (+ angle degree) *pi*) 360))) )))
  (define divisor  0)
  (define offset  0)
  (define c  0)
  (define matrix-list  0)
  (set! divisor 1)
  (set! offset (if (or (= desaturate-type 1) (= desaturate-type 3)) 128 0))
  (set! c      (if (or (= desaturate-type 1) (= desaturate-type 3))   0 1))
  (set! matrix-list (cond
    ((= type 0)
      (list         0          0          0          0         0
                    0          0 (sine  90)          0         0
                    0 (sine 180)          c (sine   0)         0
                    0          0 (sine 270)          0         0
                    0          0          0          0         0))    ; 4
    ((= type 1)
      (list         0          0          0          0         0
                    0 (sine 135) (sine  90) (sine  45)         0
                    0 (sine 180)          c (sine   0)         0
                    0 (sine 225) (sine 270) (sine 315)         0
                    0          0          0          0         0))    ; 8
    ((= type 2)
      (list          0          0 (sine  90)          0          0
                     0          0 (sine  90)          0          0
            (sine 180) (sine 180)          c (sine   0) (sine   0)
                     0          0 (sine 270)          0          0
                     0          0 (sine 270)          0          0))    ; 8
    ((= type 3)
      (list          0          0 (sine  90)          0          0
                     0 (sine 135) (sine  90) (sine  45)          0
            (sine 180) (sine 180)          c (sine   0) (sine   0)
                     0 (sine 225) (sine 270) (sine 315)          0
                     0          0 (sine 270)          0          0))    ; 12
    ((= type 4)
      (list (sine 135)          0 (sine  90)          0 (sine  45)
                     0 (sine 135) (sine  90) (sine  45)          0
            (sine 180) (sine 180)          c (sine   0) (sine   0)
                     0 (sine 225) (sine 270) (sine 315)          0
            (sine 225)          0 (sine 270)          0 (sine 315)))    ; 12
    ((= type 5)
      (list (sine 135) (sine 120) (sine  90) (sine  60) (sine  45)
            (sine 150)          0          0          0 (sine  30)
            (sine 180)          0          c          0 (sine   0)
            (sine 210)          0          0          0 (sine 330)
            (sine 225) (sine 240) (sine 270) (sine 300) (sine 315)))    ; 16
    ((= type 6)
      (list (sine 135) (sine 120) (sine  90) (sine  60) (sine  45)
            (sine 150) (sine 135) (sine  90) (sine  45) (sine  30)
            (sine 180) (sine 180)          c (sine   0) (sine   0)
            (sine 210) (sine 225) (sine 270) (sine 315) (sine 330)
            (sine 225) (sine 240) (sine 270) (sine 300) (sine 315)))    ; 24
  )) ; end of cond

  (let* ((matrix (get-matrix matrix-list))
         (channels (get-channels drawable gray? red? green? blue? alpha?))
         (listed (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg (car listed))
         (bmode (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (if (and (or (= desaturate-type 2) (= desaturate-type 3))
             (eqv? (car (gimp-image-base-type img)) RGB))
        (gimp-desaturate drawable))
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-emboss"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Emboss..."
  (string-append CONV-PRESET-DESCRIPTION "(Emboss)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Type"            '("Type 1"  "Type 2"  "Type 3"  "Type 4"
                                    "Type 5"  "Type 6"  "Type 7")
  SF-OPTION     "Color"           '("Colored Emboss" "Color Edge"
                                    "White-Black Emboss" "Gray Emboss")
  SF-ADJUSTMENT "Angle"           '(45 0 360 1 45 0 0)
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 8.0 0.5 0.2 1 0)
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Emboss variation


(define (script-fu-convolution-presets-emboss-variation
            img drawable
            type degree a
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
  (define divisor  0)
  (define offset  0)
  (define matrix-list  0)
  (set! matrix-list (cond
    ((= type 0)
      (set! divisor (*  5 a))
      (set! offset 0)
      (list        0        0        0        0        0
                   0 (* -2 a) (* -2 a) (* -2 a)        0
                   0 (* -1 a) (*  5 a) (*  1 a)        0
                   0 (*  2 a) (*  2 a) (*  2 a)        0
                   0        0        0        0        0))
    ((= type 1)
      (set! divisor 1)
      (set! offset 0)
      (list        0        0        0        0        0
                   0 (*  3 a)        0 (*  3 a)        0
                   0        0 (*  1 a)        0        0
            (* -1 a) (* -1 a) (* -1 a) (* -1 a)        0
                   0 (* -1 a)        0        0        0))
    ((= type 2)
      (set! divisor (*  1 a))
      (set! offset 0)
      (list        0        0        0        0        0
            (*  2 a)        0 (* -1 a) (* -3 a) (* -1 a)
            (*  1 a) (*  1 a) (*  1 a) (* -1 a)        0
            (*  1 a)        0 (*  1 a) (* -1 a)        0
                   0        0 (*  2 a)        0        0))
    ((= type 3)
      (set! divisor (*  2 a))
      (set! offset 0)
      (list        0        0        0        0        0
            (* -1 a)        0 (* -5 a)        0 (* -1 a)
            (*  1 a)        0        0        0 (*  1 a)
            (*  1 a)        0 (*  5 a)        0 (*  1 a)
                   0        0        0        0        0))
    ((= type 4)
      (set! divisor 1)
      (set! offset 0)
      (list        0 (* -3 a)        0 (* -3 a)        0
            (*  1 a)        0        0        0 (*  2 a)
                   0 (* -3 a)        0 (*  3 a)        0
            (*  2 a)        0        0        0        0
                   0 (* -1 a)        0 (*  3 a)        0))
    ((= type 5)
      (set! divisor 1)
      (set! offset 128)
      (list        0        0        0        0        0
                   0 (* -3 a)        0 (* -3 a)        0
                   0        0        0        0        0
                   0 (*  3 a)        0 (*  3 a)        0
                   0        0        0        0        0))
    ((= type 6)
      (set! divisor 1)
      (set! offset 128)
      (list        0        0        0        0        0
                   0 (*  1 a) (*  1 a) (*  1 a)        0
                   0 (*  1 a) (* -2 a) (*  1 a)        0
                   0 (* -1 a) (* -1 a) (* -1 a)        0
                   0        0        0        0        0))
   )) ; end of cond

  (let* ((rotated-matrix-list (get-rotated-matrix-list matrix-list degree))
         (matrix (get-matrix rotated-matrix-list))
         (channels (get-channels drawable gray? red? green? blue? alpha?))
         (listed (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg (car listed))
         (bmode (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-emboss-variation"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Emboss Variation..."
  (string-append CONV-PRESET-DESCRIPTION "(Emboss Variation)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Type"            '("Type 1" "Type 2" "Type 3" "Type 4"
                                    "Type 5" "Type 6" "Type 7")
  SF-OPTION     "Degree"          CONV-PRESET-DEGREE-LIST
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 4.0 1 1 1 0)
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Brightness-Contrast


(define (script-fu-convolution-presets-brightness-contrast
            img drawable
            brightness contrast
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((n           1)
         (b           0)
         (divisor     (abs (/ 128 contrast)))
         (offset      (+ (/ -128 (/ 128 contrast)) brightness))
         (c           (if (< contrast 0) (- divisor 1) (+ divisor 1)))
         (matrix-list (get-common-matrix-list 0 b c))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

;(gimp-message(string-append
;               "c: "       (number->string c)       "\n"
;               "divisor: " (number->string divisor) "\n"
;               "offset: "  (number->string offset)  ))
    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-brightness-contrast"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Brightness-Contrast..."
  (string-append CONV-PRESET-DESCRIPTION "(Brightness-Contrast)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-ADJUSTMENT "Brightness"      '( 0 -128 128 1 1 0 0)
  SF-ADJUSTMENT "Contrast"        '(32 -128 128 1 1 0 0)
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Posterize Effect


(define (script-fu-convolution-presets-posterize
            img drawable
            a brightness
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((n           1)
         (b           0)
         (c           a)
         (divisor     1)
         (offset      (+ (* (- a 1) -128) brightness))
         (matrix-list (get-common-matrix-list 0 b c))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-posterize"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Posterize..."
  (string-append CONV-PRESET-DESCRIPTION "(Posterize)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-ADJUSTMENT "Amount"          '(4.0 1.0 16.0 1 1 1 0)
  SF-ADJUSTMENT "Brightness"      '(0 -127 127 1 1 0 0)
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Posterize Variation


(define (script-fu-convolution-presets-posterize-variation
            img drawable
            type a
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
  (define divisor  0)
  (define offset  0)
  (define c  0)
  (define a  0)
  (define -a  0)
  (define b  0)
  (define -b  0)
  (define matrix-list  0)

  (set! matrix-list (cond
    ((= type 0)
      (set! c (* a 10))
      (set! divisor 1)
      (set! offset (- (* c 9 10)))
      (list   0   0   0   0   0
              0   0   0   0   0
              0   0   c   0   0
              0   0   0   0   0
              0   0   0   0   0))
    ((= type 1)
      (set! b (* a 10))
      (set! divisor 1)
      (set! offset (- (* b 9 10)))
      (list (* b 10)        0           0           0            0
                   0        0     (* b 5)           0            0
                   0  (* b 5)     (* b 7) (- (* b 5))            0
                   0        0 (- (* b 5))           0            0
                   0        0           0           0 (- (* b 10))))
    ((= type 2)
      (set! b (* a 2))
      (set! divisor 1)
      (set! offset (- (* a 255)))
      (list   b   0   0   0   0
              0   0   0   0   0
              0   0   0   0   0
              0   0   0   0   0
              0   0   0   0   b))
    ((= type 3)
      (set! b (* a 2))
      (set! -b (- b))
      (set! c 1)
      (set! divisor c)
      (set! offset 0)
      (list   0   0   0   0   0
              0   b  -b   b   0
              0  -b   c  -b   0
              0   b  -b   b   0
              0   0   0   0   0))
   )) ; end of cond

  (let* ((matrix (get-matrix matrix-list))
         (channels (get-channels drawable gray? red? green? blue? alpha?))
         (listed (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg (car listed))
         (bmode (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-posterize-variation"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Posterize Variation..."
  (string-append CONV-PRESET-DESCRIPTION "(Posterize Variation)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Type"            '("Type 1" "Type 2" "Type 3" "Type 4")
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Shake Effect


(define (script-fu-convolution-presets-shake
            img drawable
            type a invert?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
  (define divisor  0)
  (define offset  0)
  (define c  0)
  (define b  0)
  (define -b  0)
  (define matrix-list  0)

  (set! b (if (eqv? invert? TRUE) (- a) a))
  (set! -b (- b))
  (set! c a)
  (set! divisor c)
  (set! offset 0)
  (set! matrix-list (cond
    ((= type 0)
      (list   0   0   0   0   0
              0   b  -b   b   0
              0  -b   c  -b   0
              0   b  -b   b   0
              0   0   0   0   0))
    ((= type 1)
      (list   0   0   b   0   0
              0   0  -b   0   0
              b  -b   c  -b   b
              0   0  -b   0   0
              0   0   b   0   0))    ; needs: invert = FALSE
    ((= type 2)
      (list   b   0   0   0   b
              0  -b   0  -b   0
              0   0   c   0   0
              0  -b   0  -b   0
              b   0   0   0   b))    ; needs: invert = FALSE
    ((= type 3)
      (list   b   0  -b   0   b
              0   0   0   0   0
             -b   0   c   0  -b
              0   0   0   0   0
              b   0  -b   0   b))
    ((= type 4)
      (list   0  -b   0   b   0
              b   0   0   0  -b
              0   0   c   0   0
             -b   0   0   0   b
              0   b   0  -b   0))
    ((= type 5)
      (list   b  -b   0   0   b
              0   0   0   0  -b
              0   0   c   0   0
             -b   0   0   0   0
              b   0   0  -b   b))
    ((= type 6)
      (list   b  -b   0   b  -b
             -b   0   0   0   b
              0   0   c   0   0
              b   0   0   0  -b
             -b   b   0  -b   b))
    ((= type 7)
      (list   0  -b   0   b   0
             -b   b   0  -b   b
              0   0   c   0   0
              b  -b   0   b  -b
              0   b   0  -b   0))
    ((= type 8)
      (list   b  -b  -b   b   b
              b   0   0   0  -b
             -b   0   c   0  -b
             -b   0   0   0   b
              b   b  -b  -b   b))
   )) ; end of cond

  (let* ((matrix (get-matrix matrix-list))
         (channels (get-channels drawable gray? red? green? blue? alpha?))
         (listed (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg (car listed))
         (bmode (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-shake"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Shake..."
  (string-append CONV-PRESET-DESCRIPTION "(Shake)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Type"            '("Type 1 (Small)"
                                              "Type  2" "Type  3" "Type  4"
                                    "Type  5" "Type  6" "Type  7" "Type  8"
                                    "Type  9")
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-TOGGLE     "Nega-Posi Invert" FALSE
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Color invert


(define (script-fu-convolution-presets-color-invert
            img drawable
            type a
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
  (define divisor  0)
  (define offset  0)
  (define c  0)
  (define a  0)
  (define -a  0)
  (define matrix-list  0)
  (set! divisor 1)
  (set! matrix-list (cond
    ((= type 0)
      (set! offset 255)
      (list   0   0   0   0   0
              0   0   0   0   0
              0   0  -1   0   0
              0   0   0   0   0
              0   0   0   0   0))
    ((= type 1)
      (set! b (* a 2))
      (set! -b (- b))
      (set! c -1)
      (set! offset 255)
      (list   b   0  -b   0   b
              0   0   0   0   0
             -b   0   c   0  -b
              0   0   0   0   0
              b   0  -b   0   b))
    ((= type 2)
      (set! b (- (/ a 2)))
      (set! c (- (* (/ a 2) 24) 1))
      (set! offset 255)
      (list   b   b   b   b   b
              b   b   b   b   b
              b   b   c   b   b
              b   b   b   b   b
              b   b   b   b   b))
    ((= type 3)
      (set! b a)
      (set! -b (- b))
      (set! offset 128)
      (list   b   0   0   0   0
              0   0   0   0   0
             -b   0   0   0   b
              0   0   0   0   0
              0   0   0   0  -b))
   )) ; end of cond

  (let* ((matrix (get-matrix matrix-list))
         (channels (get-channels drawable gray? red? green? blue? alpha?))
         (listed (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg (car listed))
         (bmode (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-color-invert"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Color Invert..."
  (string-append CONV-PRESET-DESCRIPTION "(Color Invert)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Type"            '("Type 1" "Type 2" "Type 3" "Type 4")
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Artistic Effects


(define (script-fu-convolution-presets-artistic
            img drawable
            type a detail-level blur?
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((detail      (* (/ 25 detail-level) (/ a 8)))
         (n           (car (last (get-common-matrix-list type 0 0))))
         (b           (- (/ a n)))
         (c           (+ a detail))
         (divisor     detail)
         (offset      128)
         (matrix-list (get-common-matrix-list type b c))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

    (gimp-image-undo-group-start img)
    (if (eqv? blur? TRUE)
        (script-fu-convolution-presets-blur
            img drawable
            type detail 1    ; use mean filter
            alpha_alg? b_mode
            gray? red? green? blue? alpha?))
    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-artistic"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Artistic..."
  (string-append CONV-PRESET-DESCRIPTION "(Artistic)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Type"            CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(8 1 64 1 1 0 0)
  SF-ADJUSTMENT "Detail Level"    '(25 1 100 1 1 0 0)
  SF-TOGGLE     "Add Blur"        FALSE
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Smudge (Mean Filter?)
;; see
;;   http://g-trip.pobox.ne.jp/TipsPsp/Logo/Tip2-5/Tip2-5.html


(define (script-fu-convolution-presets-smudge
            img drawable
            type a repeat
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (let* ((i           0)
         (n           (car (last (get-common-matrix-list type 0 0))))
         (b           a)
         (c           a)
         (divisor     (+ (* b n) c))
         (offset      0)
         (matrix-list (get-common-matrix-list type b c))
         (matrix      (get-matrix matrix-list))
         (channels    (get-channels drawable gray? red? green? blue? alpha?))
         (listed      (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

    (gimp-image-undo-group-start img)
    (while (< i repeat)
      (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                          divisor offset argc_channles channels bmode)
      (set! i (+ i 1)))
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-smudge"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Smudge..."
  (string-append CONV-PRESET-DESCRIPTION "(Smudge)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Type"            CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(8 1 64 1 1 0 0)
  SF-ADJUSTMENT "Repeat"          '(1 1 64 1 1 0 1)
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Others


(define (script-fu-convolution-presets-others
            img drawable
            type
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)

  (define divisor  0)
  (define offset  0)
  (define matrix-list  0)

  (set! matrix-list (cond
    ((= type 0)
      (set! divisor 1)
      (set! offset -7000)
      (list   0   0  100   0   0
              0   0    0   0   0
            100   0 -330   0 100
              0   0    0   0   0
              0   0  100   0   0))
    ((= type 1)
      (set! divisor 10)
      (set! offset 290)
      (list   0   1   0   0   0
              1   2   1   0   0
             -2   0  -2   0  -1
              0   0   0  -2   0
              0   0  -1   0  -3))
    ((= type 2)
      (set! divisor 1)
      (set! offset 100)
      (list  -1  -1  -1  -1  -1
             -1  -1  -1  -1  -1
             -1  -1  24  -1  -1
             -1  -1  -1  -1  -1
             -1  -1  -1  -1  -1))
    ((= type 3)
      (set! divisor 1)
      (set! offset 0)
      (list   1   1   1   1   1
              1   1   1   1   1
              1   1 -23   1   1
              1   1   1   1   1
              1   1   1   1   1))
    ((= type 4)
      (set! divisor 1)
      (set! offset 0)
      (list   0   0   0   0   0
              0   0   1   3   1
              0  -1  -1   1   0
              0  -2  -1   1   0
              0  -2   0   1   0))
    ((= type 5)
      (set! divisor 1)
      (set! offset 0)
      (list  -1   0   6   0  -3
              0   0   0   0   0
              4   0  -7   0   4
              0   0   0   0   0
             -3   0   6   0  -5))
    ((= type 6)
      (set! divisor 1)
      (set! offset 128)
      (list   0   0   0   0   0
              0   1   1   0   0
              0   1   0  -1   0
              0   0  -1  -1   0
              0   0   0   0   0))
    ((= type 7)
      (set! divisor 1)
      (set! offset 0)
      (list   0   0   0   0   0
              0   1   1   0   0
              0   1   1  -1   0
              0   0  -1  -1   0
              0   0   0   0   0))
    ((= type 8)
      (set! divisor 1)
      (set! offset 0)
      (list   0   2   0   0   1
              2   0  -4   0   0
              0  -5   5   5   0
              0   0  -4   0   0
              0   0  -2   0   0))
    ((= type 9)
      (set! divisor 1)
      (set! offset 128)
      (list   0   0   0   0   0
              0  -1  -1  -1   0
              0  -1   8  -1   0
              0  -1  -1  -1   0
              0   0   0   0   0))
    ((= type 10)
      (set! divisor 1)
      (set! offset -128)
      (list   0   0   0   0   0
              0   1   1   1   0
              0   1  -6   1   0
              0   1   1   1   0
              0   0   0   0   0))
    ((= type 11)
      (set! divisor 49)
      (set! offset 0)
      (list   9   0   3   0   9
              0   0   0   0   0
              3   0   1   0   3
              0   0   0   0   0
              9   0   3   0   9))
    ((= type 12)
      (set! divisor 1)
      (set! offset 0)
      (list   0   0   0   0   0
             -1   0  -5   0  -1
              1   0   0   0   1
              1   0   5   0   1
              0   0   0   0   0))
    ((= type 13)
      (set! divisor 2)
      (set! offset 50)
      (list   0  -3   0  -3   0
              1   0   0   0   2
              0  -3   1   3   0
              2   0   0   0   0
              0  -1   0   3   0))
    ((= type 14)
      (set! divisor 1)
      (set! offset 0)
      (list   0   0   0   0   0
              0   1   1   1   0
              0   1  -7   1   0
              0   1   1   1   0
              0   0   0   0   0))
    ((= type 15)
      (set! divisor 1)
      (set! offset 0)
      (list  -1  -1  -1  -1  -1
             -1 -10 -10 -10  -1
             -1 -10  98 -10  -1
             -1 -10 -10 -10  -1
             -1  -1  -1  -1  -1))
    ((= type 16)
      (set! divisor 1)
      (set! offset 0)
      (list   0  -1  -2  -3  -4
              0  -1   3   2   1
              0  -1  10   2   1
              0  -1   3   2   1
              0  -1  -2  -3  -4))
    ((= type 17)
      (set! divisor 1)
      (set! offset 0)
      (list   1   1   0  -1  -1
              1   1   0  -1  -1
              0   0   1   0   0
             -1  -1   0   1   1
             -1  -1   0   1   1))
    ((= type 18)
      (set! divisor 7)
      (set! offset 0)
      (list  -2  -2   2   2   2
             -2  -1   1  -1   2
             -2   1   4   1   2
             -2  -1   1  -1  -1
              3   3  -3  -3   4))
    ('else
      (set! divisor 1)
      (set! offset 0)
      (list   0   0   0   0   0
              0   0   0   0   0
              0   0   1   0   0
              0   0   0   0   0
              0   0   0   0   0))
   )) ; end of cond

  (let* ((matrix (get-matrix matrix-list))
         (channels (get-channels drawable gray? red? green? blue? alpha?))
         (listed (get-alpha_alg-bmode drawable alpha_alg? b_mode))
         (alpha_alg (car listed))
         (bmode (cadr listed)))

    (plug-in-convmatrix 1 img drawable argc_matrix matrix alpha_alg
                        divisor offset argc_channles channels bmode)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-others"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Others..."
  (string-append CONV-PRESET-DESCRIPTION "(Others)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Type"            '("Type 1" "Type 2" "Type 3" "Type 4"
                                    "Type 5" "Type 6" "Emboss" "Colored Emboss"
                                    "Edge Lighting" "Sharp" "Edge Detection"
                                    "Blur with Direction" "Type 13" "Type 14"
                                    "Grease" "Lithograph" "Distortion"
                                    "Type 18" "Type 19")
  SF-TOGGLE     "Alpha-weighting" TRUE
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
  SF-TOGGLE     "Gray Channel"    FALSE
  SF-TOGGLE     "Red Channel"     TRUE
  SF-TOGGLE     "Green Channel"   TRUE
  SF-TOGGLE     "Blue Channel"    TRUE
  SF-TOGGLE     "Alpha Channel"   FALSE)









    ;; Random Color


(define (script-fu-convolution-presets-random-color
            img drawable
            type a
            b_mode)

  (let* ((n           (car (last (get-common-matrix-list type 0 0))))
         (b           1)
         (c           1)
         (divisor     0)
         (offset      0)
         (matrix-list (get-common-matrix-list type b c))
         (listed      (get-alpha_alg-bmode drawable FALSE b_mode))
         (alpha_alg   (car listed))
         (bmode       (cadr listed)))

;(conv-preset-debug-output matrix channels alpha_alg bmode)
;(gimp-message (number->string divisor))
         (if (eqv? (car (gimp-drawable-is-rgb drawable)) FALSE)
             (gimp-message "Can apply on RGB* Layer")
             (while (<= b 3)
               (let* ((layer (car (gimp-layer-copy drawable TRUE)))
                      (matrix-list2
                        (mapcar
                          (lambda (x)
                            (let* ((value (* a (rand (* 100 (+ x 1))) 0.01)))
                              (set! divisor (+ divisor value))
                              value))
                          (butlast matrix-list)))
                      (matrix   (get-matrix matrix-list2))
                      (channels (get-channels layer FALSE (fmod (+ b 1) 3)
                                              (fmod (+ b 2) 3) (fmod (+ b 3) 3)
                                              FALSE)))
                 (gimp-image-add-layer img layer -1)
                 (plug-in-convmatrix 1 img layer argc_matrix matrix alpha_alg
                                    divisor offset argc_channles channels bmode)
                 (gimp-layer-set-mode layer DIVIDE-MODE)
                 (set! b (+ b 1)))))

    (gimp-displays-flush)))

(script-fu-register
  "script-fu-convolution-presets-random-color"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Random Color (test)..."
  (string-append CONV-PRESET-DESCRIPTION "(Random Color)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Matrix Type"     CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-OPTION     "Edge"            CONV-PRESET-EDGE-LIST
)









    ;; Convolution Effects (prototype)


(define (script-fu-convolution-presets-effects-test
            img drawable
            type a effects-type
)

  (if (equal? (car (gimp-drawable-is-layer drawable)) FALSE)
      (gimp-message "Can not apply this script to Channel or Layer Mask")
      (let* ((old-layer-mode (car (gimp-layer-get-mode drawable)))
             (old-layer-opacity (car (gimp-layer-get-opacity drawable)))
             (dummy (gimp-layer-set-mode drawable NORMAL-MODE))
             (dummy (gimp-layer-set-opacity drawable 100))
             (layer (car (gimp-layer-copy drawable TRUE)))
             (image-is-rgb (if (eqv? (car (gimp-image-base-type img)) RGB) #t #f))
             (alpha_alg? TRUE)
             (b_mode 0)    ; Extend
             (gray?  (if image-is-rgb FALSE TRUE))
             (red?   (if image-is-rgb TRUE FALSE))
             (green? (if image-is-rgb TRUE FALSE))
             (blue?  (if image-is-rgb TRUE FALSE))
             (alpha? FALSE)
             (final  0)
            )
        (gimp-image-undo-group-start img)
        (gimp-image-add-layer img layer -1)

        (cond

          ((= effects-type 0)
              (gimp-desaturate layer)
              (script-fu-convolution-presets-posterize
            img layer
            a 0
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
              (script-fu-convolution-presets-edge-detect-laplace
            img layer
            type (sqrt a) 0 FALSE FALSE TRUE
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
              (gimp-layer-set-mode layer VALUE-MODE))

          ((= effects-type 1)
              (gimp-desaturate layer)
              (script-fu-convolution-presets-blur
            img layer
            type (sqrt a) 0
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
              (script-fu-convolution-presets-edge-detect-laplace
            img layer
            type (sqrt a) 0 FALSE FALSE TRUE
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
              (gimp-layer-set-mode layer VALUE-MODE))

          ((= effects-type 2)
              (script-fu-convolution-presets-edge-detect
            img layer
            type (* 2 a) 0
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
              (gimp-layer-set-mode layer DIVIDE-MODE))

          ((= effects-type 3)
              (define (repeat-call x)
                (script-fu-convolution-presets-sharp
            img layer
            type (* a x)
            alpha_alg? b_mode
            gray? red? green? blue? alpha?)
                (if (< x 4)
                    (repeat-call (+ x 1))))
              (repeat-call 1)
              (gimp-layer-set-mode layer OVERLAY-MODE))

        ) ; end of cond

        (if (not (= (car (gimp-layer-get-mask drawable)) -1))
            (gimp-message "Layer mask had been applied into Layer"))

        (set! final (car (gimp-image-merge-down img layer EXPAND-AS-NECESSARY)))
        (gimp-layer-set-mode final old-layer-mode)
        (gimp-layer-set-opacity final old-layer-opacity)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush))))

(script-fu-register
  "script-fu-convolution-presets-effects-test"
  "<Image>/FX-Foundry/Convolution Matrix Presets/Effects/Test..."
  (string-append CONV-PRESET-DESCRIPTION "(Effects)")
  "Iccii <iccii@hotmail.com>"
  CONV-PRESET-COPYRIGHT
  CONV-PRESET-DATE
  CONV-PRESET-IMAGE-TYPE
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-OPTION     "Type"            CONV-PRESET-TYPE-LIST
  SF-ADJUSTMENT "Amount"          '(1.0 0.1 16.0 1 1 1 0)
  SF-OPTION     "Effects"         '("no name1 (edge)" "no name2(edge)"
                                    "no name3" "no name4 (noisify)")
)


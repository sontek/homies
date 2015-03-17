;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Colortemp  script  for GIMP 2.4
; Copyright (C) 2006 Luca de Alfaro <lda@dealfaro.com>
;
; Tags: photo, color correction, color temperature
;
; Author statement:
; Converts the color temperature of an image
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


(define (script-fu-colortemp image drawable sourcemod sourcetemp desttemp intensmult satura)

  (define (floor x) (- x (fmod x 1)))

; Linear interpolations, used by the table lookup below
  (define (interpolate xlow xhi ylow yhi x)
    (let* ((deltax (- xhi xlow))
       (deltap (- x xlow))
       (deltay (- yhi ylow))
       (inc    (* deltay (/ deltap deltax)))
       )
      (+ ylow inc)))


; Interpolates colors
  (define (interpolate-rows low high temp)
    (let* ((t_low (car    low))
       (r_low (cadr   low))
       (g_low (caddr  low))
       (b_low (nth 3  low))
       (t_hi  (car    high))
       (r_hi  (cadr   high))
       (g_hi  (caddr  high))
       (b_hi  (nth 3  high)))
      (list
       (interpolate t_low t_hi r_low r_hi temp)
       (interpolate t_low t_hi g_low g_hi temp)
       (interpolate t_low t_hi b_low b_hi temp))))


; Given the colortable and color temperature in K, finds
; r, g, b values in the linear space.
  (define (findcolors colortable temp)
    (let* ((l colortable)
       (low (car colortable)))
      (while (> temp (car (car l)))
         (set! low (car l))
             (set! l   (cdr l)))
      (interpolate-rows low (car l) temp)))

; Converts from linear to gamma-corrected sRGB [0,1]
  (define (togamma x)
    (if (<= x 0.00304)
    (* x 12.92)
    (let* ((exponent (/ 1 2.4)))
      (- (* 1.055 (pow x exponent)) 0.055))))

; Converts from linear to gamma-corrected sRGB [0,255]
  (define (togamma255 x)
    (max (min (floor (* 256 (togamma x))) 255) 0))

; Converts from gamma-corrected sRGB [0,1] to linear
  (define (tolinear y)
    (if (<= y 0.0392768)
    (/ y 12.92)
    (let* ((ratio (/ (+ y 0.055) 1.055)))
      (pow ratio 2.4))))

; Converts from gamma-corrected sRGB [0,255] to linear
  (define (to255linear y)
    (tolinear (/ y 255)))

; Applies a ratio (in linear space) to sRGB values, where the sRGB
; values are scaled 0..255.
  (define (lin-mult-in-gamma-space y255 r)
    (togamma255 (* r (to255linear y255))))

; Finds the colors (in linear space) of the temperature whose color
; best matches the foreground.  It uses a least-square fit (any better ideas?).
  (define (sourcecolors-from-bestmatch colortable fg)
    (let* ((fg-red   (to255linear (car   fg)))
       (fg-green (to255linear (cadr  fg)))
       (fg-blue  (to255linear (caddr fg)))
       (m (max fg-red (max fg-green fg-blue))))
      (if (< m 0.000001)
      (list 1 1 1)
      (let* ((nr (min 1 (/ fg-red   m)))
         (ng (min 1 (/ fg-green m)))
         (nb (min 1 (/ fg-blue  m)))
         (t 1000)
         (d 4)
         (best (list 1 1 1)))
        (while (<= t 12000)
           (let* ((c (findcolors colortable t))
              (cr (car   c))
              (cg (cadr  c))
              (cb (caddr c))
              (dr (- cr nr))
              (dg (- cg ng))
              (db (- cb nb))
              (d1 (+ (* dr dr) (+ (* dg dg) (* db db)))))
             (if (< d1 d)
             (set! best c))
             (if (< d1 d)
             (set! d d1)))
           (set! t (+ t 50))) ; we compute the temp within 50K; should be enough
        best))))

; Finds the colors (in linear space) of the foreground.
  (define (sourcecolors-from-fg fg)
    (let* ((fg-red   (to255linear (car   fg)))
       (fg-green (to255linear (cadr  fg)))
       (fg-blue  (to255linear (caddr fg)))
       (m (max fg-red (max fg-green fg-blue))))
      (if (< m 0.000001)
      (list 1 1 1)
      (let* ((nr (min 1 (/ fg-red   m)))
         (ng (min 1 (/ fg-green m)))
         (nb (min 1 (/ fg-blue  m))))
        (list nr ng nb)))))

; This is the color table, taken from
; http://www.vendian.org/mncharity/dir3/blackbody/UnstableURLs/bbr_color.html
    (let* (
           (colortemp     (list
             (list  1000 1.0000 0.0401 0.0000)
             (list  1100 1.0000 0.0631 0.0000)
             (list  1200 1.0000 0.0860 0.0000)
             (list  1300 1.0000 0.1085 0.0000)
             (list  1400 1.0000 0.1303 0.0000)
             (list  1500 1.0000 0.1515 0.0000)
             (list  1600 1.0000 0.1718 0.0000)
             (list  1700 1.0000 0.1912 0.0000)
             (list  1800 1.0000 0.2097 0.0000)
             (list  1900 1.0000 0.2272 0.0000)
             (list  2000 1.0000 0.2484 0.0061)
             (list  2100 1.0000 0.2709 0.0153)
             (list  2200 1.0000 0.2930 0.0257)
             (list  2300 1.0000 0.3149 0.0373)
             (list  2400 1.0000 0.3364 0.0501)
             (list  2500 1.0000 0.3577 0.0640)
             (list  2600 1.0000 0.3786 0.0790)
             (list  2700 1.0000 0.3992 0.0950)
             (list  2800 1.0000 0.4195 0.1119)
             (list  2900 1.0000 0.4394 0.1297)
             (list  3000 1.0000 0.4589 0.1483)
             (list  3100 1.0000 0.4781 0.1677)
             (list  3200 1.0000 0.4970 0.1879)
             (list  3300 1.0000 0.5155 0.2087)
             (list  3400 1.0000 0.5336 0.2301)
             (list  3500 1.0000 0.5515 0.2520)
             (list  3600 1.0000 0.5689 0.2745)
             (list  3700 1.0000 0.5860 0.2974)
             (list  3800 1.0000 0.6028 0.3207)
             (list  3900 1.0000 0.6193 0.3444)
             (list  4000 1.0000 0.6354 0.3684)
             (list  4100 1.0000 0.6511 0.3927)
             (list  4200 1.0000 0.6666 0.4172)
             (list  4300 1.0000 0.6817 0.4419)
             (list  4400 1.0000 0.6966 0.4668)
             (list  4500 1.0000 0.7111 0.4919)
             (list  4600 1.0000 0.7253 0.5170)
             (list  4700 1.0000 0.7392 0.5422)
             (list  4800 1.0000 0.7528 0.5675)
             (list  4900 1.0000 0.7661 0.5928)
             (list  5000 1.0000 0.7792 0.6180)
             (list  5100 1.0000 0.7919 0.6433)
             (list  5200 1.0000 0.8044 0.6685)
             (list  5300 1.0000 0.8167 0.6937)
             (list  5400 1.0000 0.8286 0.7187)
             (list  5500 1.0000 0.8403 0.7437)
             (list  5600 1.0000 0.8518 0.7686)
             (list  5700 1.0000 0.8630 0.7933)
             (list  5800 1.0000 0.8740 0.8179)
             (list  5900 1.0000 0.8847 0.8424)
             (list  6000 1.0000 0.8952 0.8666)
             (list  6100 1.0000 0.9055 0.8907)
             (list  6200 1.0000 0.9156 0.9147)
             (list  6300 1.0000 0.9254 0.9384)
             (list  6400 1.0000 0.9351 0.9619)
             (list  6500 1.0000 0.9445 0.9853)
             (list  6600 0.9917 0.9458 1.0000)
             (list  6700 0.9696 0.9336 1.0000)
             (list  6800 0.9488 0.9219 1.0000)
             (list  6900 0.9290 0.9107 1.0000)
             (list  7000 0.9102 0.9000 1.0000)
             (list  7100 0.8923 0.8897 1.0000)
             (list  7200 0.8753 0.8799 1.0000)
             (list  7300 0.8591 0.8704 1.0000)
             (list  7400 0.8437 0.8614 1.0000)
             (list  7500 0.8289 0.8527 1.0000)
             (list  7600 0.8149 0.8443 1.0000)
             (list  7700 0.8014 0.8363 1.0000)
             (list  7800 0.7885 0.8285 1.0000)
             (list  7900 0.7762 0.8211 1.0000)
             (list  8000 0.7644 0.8139 1.0000)
             (list  8100 0.7531 0.8069 1.0000)
             (list  8200 0.7423 0.8002 1.0000)
             (list  8300 0.7319 0.7938 1.0000)
             (list  8400 0.7219 0.7875 1.0000)
             (list  8500 0.7123 0.7815 1.0000)
             (list  8600 0.7030 0.7757 1.0000)
             (list  8700 0.6941 0.7700 1.0000)
             (list  8800 0.6856 0.7645 1.0000)
             (list  8900 0.6773 0.7593 1.0000)
             (list  9000 0.6693 0.7541 1.0000)
             (list  9100 0.6617 0.7492 1.0000)
             (list  9200 0.6543 0.7444 1.0000)
             (list  9300 0.6471 0.7397 1.0000)
             (list  9400 0.6402 0.7352 1.0000)
             (list  9500 0.6335 0.7308 1.0000)
             (list  9600 0.6271 0.7265 1.0000)
             (list  9700 0.6208 0.7224 1.0000)
             (list  9800 0.6148 0.7183 1.0000)
             (list  9900 0.6089 0.7144 1.0000)
             (list 10000 0.6033 0.7106 1.0000)
             (list 10100 0.5978 0.7069 1.0000)
             (list 10200 0.5925 0.7033 1.0000)
             (list 10300 0.5873 0.6998 1.0000)
             (list 10400 0.5823 0.6964 1.0000)
             (list 10500 0.5774 0.6930 1.0000)
             (list 10600 0.5727 0.6898 1.0000)
             (list 10700 0.5681 0.6866 1.0000)
             (list 10800 0.5637 0.6836 1.0000)
             (list 10900 0.5593 0.6806 1.0000)
             (list 11000 0.5551 0.6776 1.0000)
             (list 11100 0.5510 0.6748 1.0000)
             (list 11200 0.5470 0.6720 1.0000)
             (list 11300 0.5432 0.6693 1.0000)
             (list 11400 0.5394 0.6666 1.0000)
             (list 11500 0.5357 0.6640 1.0000)
             (list 11600 0.5322 0.6615 1.0000)
             (list 11700 0.5287 0.6590 1.0000)
             (list 11800 0.5253 0.6566 1.0000)
             (list 11900 0.5220 0.6542 1.0000)
             (list 12000 0.5187 0.6519 1.0000)

             ))

     ; Foreground
         (fg (car (gimp-context-get-foreground)))
     ; Finds the linear source colors
                         ; The colors are taken from slider temperature
     (sourcecolors (cond ((= sourcemod 0) (findcolors colortemp sourcetemp))
                 ; The colors are taken from foreground best-match
                 ((= sourcemod 1) (sourcecolors-from-bestmatch colortemp fg))))
     ; and the target colors
     (targetcolors (findcolors colortemp desttemp))
     ; computes the ratios
     (rr (/ (car   targetcolors) (car   sourcecolors)))
     (rg (/ (cadr  targetcolors) (cadr  sourcecolors)))
     (rb (/ (caddr targetcolors) (caddr sourcecolors)))
     ; Multiplies them by the intensity modification
     (m (/ intensmult 100))
     ; And these are the real ratios
     (rratio (* rr m))
     (gratio (* rg m))
     (bratio (* rb m))

         (i 0)
         (num_bytes 256)
         (red-curve   (cons-array num_bytes 'byte))
         (green-curve (cons-array num_bytes 'byte))
         (blue-curve  (cons-array num_bytes 'byte)))

    (gimp-image-undo-group-start image)

    (while (< i num_bytes)
      (aset red-curve   i (lin-mult-in-gamma-space i rratio))
      (aset green-curve i (lin-mult-in-gamma-space i gratio))
      (aset blue-curve  i (lin-mult-in-gamma-space i bratio))
      (set! i (+ i 1)))

    (gimp-curves-explicit drawable RED-LUT   num_bytes red-curve  )
    (gimp-curves-explicit drawable GREEN-LUT num_bytes green-curve)
    (gimp-curves-explicit drawable BLUE-LUT  num_bytes blue-curve )

    (gimp-hue-saturation drawable 0 0.0 0.0 satura)

    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
))

(script-fu-register
  "script-fu-colortemp"
  _"Convert Color Temperat_ure"
  "Convert Color Temperature 2.0\n\nFor help, go to http://www.dealfaro.org/home/code/colortemp.html\n"
  "Luca de Alfaro <lda@dealfaro.com>"
  "Luca de Alfaro"
  "December 2006"
  "RGB*"
  SF-IMAGE    "Image"         0
  SF-DRAWABLE "Drawable"      0
  SF-OPTION      "Obtain original temperature" '("From slider below" "From foreground color")
  SF-ADJUSTMENT _"Original temperature (K)" '(6500 1000 12000 25 250 0 0)
  SF-ADJUSTMENT _"Target temperature (K)"   '(6500 1000 12000 25 250 0 0)
  SF-ADJUSTMENT _"Intensity (%)" '(100 0 200 1 10 0 0)
  SF-ADJUSTMENT _"Saturation change (%)" '(0 -100 100 1 10 0 0)
)

(script-fu-menu-register "script-fu-colortemp"
             _"<Image>/FX-Foundry/Color")


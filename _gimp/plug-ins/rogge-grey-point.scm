;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Grey point script  for GIMP 2.4
; Copyright (C) 2004 Dr. Martin Rogge <marogge@onlinehome.de>
; based on:
; White/Black balance script  for GIMP 1.2
; Copyright (C) 2002 Iccii <iccii@hotmail.com>
;
; Tags: photo, whitebalance, grey point
;
; Author statement: flexible white balance correction tool
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Version 2.0
; Last changed: 15.02.2007
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

(define (script-mr-grey-point image drawable mode )

  (define (round x) (trunc (+ x 0.5)))

  (define (interpolate source target i)
        (if (= source 0) target (max (min (round (* (/ target source) i))  255) 0)))

  (let* ((fg           (car (gimp-context-get-foreground)))
         (bg           (car (gimp-context-get-background)))

         (source_red   (car   fg))
         (source_green (cadr  fg))
         (source_blue  (caddr fg))

         (average      (round (/ (+ source_red source_green source_blue) 3)))

         (target_red   (cond ((= mode 0) average)
                             ((= mode 1) 255)
                             ((= mode 2) (car bg))
                        ))
         (target_green (cond ((= mode 0) average)
                             ((= mode 1) 255)
                             ((= mode 2) (cadr bg))
                        ))
         (target_blue  (cond ((= mode 0) average)
                             ((= mode 1) 255)
                             ((= mode 2) (caddr bg))
                        ))

         (i             0)

         (num_bytes     256)

         (red-curve    (cons-array num_bytes 'byte))
         (green-curve  (cons-array num_bytes 'byte))
         (blue-curve   (cons-array num_bytes 'byte)))

    (gimp-image-undo-group-start image)

    (while (< i num_bytes)
      (aset red-curve   i (interpolate source_red   target_red   i))
      (aset green-curve i (interpolate source_green target_green i))
      (aset blue-curve  i (interpolate source_blue  target_blue  i))
      (set! i (+ i 1)))

    (gimp-curves-explicit drawable RED-LUT   num_bytes red-curve  )
    (gimp-curves-explicit drawable GREEN-LUT num_bytes green-curve)
    (gimp-curves-explicit drawable BLUE-LUT  num_bytes blue-curve )

    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
))

(script-fu-register
  "script-mr-grey-point"
  "<Image>/FX-Foundry/Color/Grey Point"
  "Linear conversion of RGB colour space.\n\nThe purpose of this script is to transform the foreground colour (as chosen with the colour pick tool) to a chosen target colour. The entire RGB colour space is linearly converted to achieve the desired mapping of the foreground colour. There are three possible choices for the target colour:\n(a) a grey colour of the same density as the foreground colour (desaturation)\n(b) white\n(c) the background colour (as chosen with the colour pick tool)\n\nThe main application area of this script should be the correction of the colour temperature of digital images.\n\nYour feedback is welcome. ;^)"
  "Dr. Martin Rogge <marogge@onlinehome.de>"
  "Dr. Martin Rogge"
  "29/09/2004 to 15/02/2007"
  "RGB*"
  SF-IMAGE    "Image"         0
  SF-DRAWABLE "Drawable"      0
  SF-OPTION   "Foreground transformation"  '("Desaturation" "White" "Background")
)


;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Midtone Sharp script  for GIMP 2.4
; Original author: Tim Jacobs <twjacobs@gmail.com>
;
; Tags: photo, sharpen
;
; Author statement: Sharpens the midtones of an image
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Last changed: 03/20/2005
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

(define (script-fu-midtone-sharp image
                                 drawable
        )

       (define (floor x)
         (- x (fmod x 1))
       )

       (define (interpolate run rise x)
         (max (min (floor (* (/ rise run) x)) 255) 0)
       )

       ; Initialize variables
       (let*
         (
           (i 0)
           (mask_opacity 50)
           (num_bytes 256)
           (thresh_1 85)
           (thresh_2 116)
           (thresh_3 140)
           (thresh_4 171)
           (thresh_5 256)
           (value-curve (cons-array num_bytes 'byte))
           (sharp-layer 0)
           (sharp-mask 0)
         )

         (gimp-image-undo-group-start image)

       ; create TRC for sharp layer mask
         (while (< i thresh_1)
           (aset value-curve i 0)
           (set! i (+ i 1))
         )

         (while (< i thresh_2)
           (aset value-curve i (interpolate (- thresh_2 thresh_1) 255 (- i thresh_1)))
           (set! i (+ i 1))
         )

         (while (< i thresh_3)
           (aset value-curve i 255)
           (set! i (+ i 1))
         )

         (while (< i thresh_4)
           (aset value-curve i (interpolate (- thresh_4 thresh_3) -255 (- i thresh_3)))
           (set! i (+ i 1))
         )

         (while (< i thresh_5)
           (aset value-curve i 0)
           (set! i (+ i 1))
         )

       ; Create new layer and add to the image
         (set! sharp-layer (car (gimp-layer-copy drawable 1)))
         (gimp-image-add-layer image sharp-layer -1)
         (gimp-drawable-set-name sharp-layer "Sharp Mask")

       ; create mask layer
         (set! sharp-mask (car (gimp-layer-create-mask sharp-layer ADD-COPY-MASK)))
         (gimp-layer-add-mask sharp-layer sharp-mask)
         (gimp-layer-set-opacity sharp-layer mask_opacity)
         (gimp-layer-set-mode sharp-layer NORMAL-MODE)

       ; apply TRC to mask layer
         (gimp-curves-explicit sharp-mask HISTOGRAM-VALUE num_bytes value-curve)

       ; apply unsharp mask to new layer
         (plug-in-unsharp-mask 1 image sharp-layer 5.5 0.75 0)

       ; Cleanup
         (gimp-image-undo-group-end image)
         (gimp-displays-flush)
       )

)

(script-fu-register "script-fu-midtone-sharp"
                    _"<Image>/FX-Foundry/Photo/Enhancement/Midtone-Sharp image"
                    "Sharpen the midtones of an image"
                    "twjacobs@gmail.com"
                    "Tim Jacobs"
                    "March 19, 2005"
                    ""
                    SF-IMAGE "Image" 0
                    SF-DRAWABLE "Drawable" 0
)

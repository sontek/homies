;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Press print script  for GIMP 2.4
; Original author: Tim Jacobs <twjacobs@gmail.com>
;
; Tags: artistic
;
; Author statement: Make a stamp form the image
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Last changed: 03/26/2005
; 20.11.2007 - added displacement size control by Alexia Death.
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

(define (script-fu-stamp image
                         drawable
                         Ink-color
                         disp
        )
       (let* (
               (blur-layer 0)
               (lineScreen1-layer 0)
               (lineScreen2-layer 0)
               (engrave-layer 0)
               (color-layer 0)
               (color-mask 0)
             )

         (gimp-image-undo-group-start image)

       ; If image in not grayscale - make it grayscale
         (if  (not (= (car (gimp-drawable-is-gray drawable)) 0)) () (gimp-image-convert-grayscale image))

       ; Create new blur layer and add to the image
         (set! blur-layer (car (gimp-layer-copy drawable 1)))
         (gimp-image-add-layer image blur-layer -1)
         (gimp-drawable-set-name blur-layer "blur")
         (plug-in-gauss-iir 1 image blur-layer 35 TRUE TRUE)

       ; Create new line screen layer and add to the image
         (set! lineScreen1-layer (car (gimp-layer-copy drawable 1)))
         (gimp-image-add-layer image lineScreen1-layer -1)
         (gimp-drawable-set-name lineScreen1-layer "line screen 1")
         (gimp-levels lineScreen1-layer HISTOGRAM-VALUE 0 213 1 0 255)
         (plug-in-newsprint 1 image lineScreen1-layer 4 2 0 45 1 45 1 45 1 45 1 15)
         (plug-in-displace 1 image lineScreen1-layer 0 15 FALSE TRUE blur-layer blur-layer 1)

       ; Create another line screen layer and add to the image
         (set! lineScreen2-layer (car (gimp-layer-copy drawable 1)))
         (gimp-image-add-layer image lineScreen2-layer -1)
         (gimp-drawable-set-name lineScreen2-layer "line screen 2")
         (gimp-levels lineScreen2-layer HISTOGRAM-VALUE 0 201 1 0 255)
         (plug-in-newsprint 1 image lineScreen2-layer 4 2 0 -45 1 45 1 45 1 45 1 15)
         (plug-in-displace 1 image lineScreen1-layer 0 disp FALSE TRUE blur-layer blur-layer 1)
         (gimp-layer-set-mode lineScreen2-layer DARKEN-ONLY-MODE)

      ;  Create and color the screen image
         (set! engrave-layer (car (gimp-image-merge-down image lineScreen2-layer EXPAND-AS-NECESSARY)))
         (gimp-drawable-set-name engrave-layer "engraved")
         (set! color-layer (car (gimp-layer-copy engrave-layer 1)))
         (gimp-image-add-layer image color-layer -1)
         (gimp-drawable-set-name color-layer "Ink Color")
         (gimp-invert color-layer)
         (set! color-mask (car (gimp-layer-create-mask color-layer ADD-COPY-MASK)))
         (gimp-layer-add-mask color-layer color-mask)
         (gimp-image-convert-rgb image)
         (gimp-context-set-background Ink-color)
         (gimp-drawable-fill color-layer BACKGROUND-FILL)

       ; Cleanup
         (gimp-image-remove-layer image blur-layer)
         (gimp-image-undo-group-end image)
         (gimp-displays-flush)
    )
)

(script-fu-register "script-fu-stamp"
                    _"<Image>/FX-Foundry/Artistic/_Press print..."
                    "Make a stamp from the image"
                    "twjacobs@gmail.com"
                    "Tim Jacobs"
                    "March 26, 2005"
                    ""
                    SF-IMAGE "Image" 0
                    SF-DRAWABLE "Drawable" 0
                    SF-COLOR "Ink Color" '(96 128 59)
                    SF-ADJUSTMENT _"Displacement size"  '(15 1 255 1 10 0 0)
)

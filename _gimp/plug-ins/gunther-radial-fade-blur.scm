;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Radial Fade Blur script  for GIMP 2.4
; Created by Gunther
;
; Tags: blur
;
; Author statement:
;
; Add a radialy fading blur effect.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; v0.01     - 16-Nov-03 - Works as intended, but lacking some options
;  0.01a     - 16-Nov-03 - Now undo function works as a block
;  0.02        - 17-Nov-03 - Blur now centred on x,y coordinates instead of top left corner
;  0.03                    - Removed layering of effect, added feather option
;  0.04                    - Doubled radius figure for use as diameter in ellipse selection
;  0.05        - 26-Nov-03 - Removed automatic feathering/made feather variable larger
;  0.06        - 22-Feb-04 - Deprecated PDB calls updated to fit with 2.0pre2
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


; set up the basics
(define (script-fu-radial-fade-blur
                img
                drawable
                amount
                feather
                radius
                posx
                posy
)

; undo grouping start
(begin
(gimp-image-undo-group-start img)

; make a feathered elliptical selection centred on the x,y coordinate
(gimp-ellipse-select img (- posx radius) (- posy radius) (* radius 2) (* radius 2) 2 1 0 0)
; invert the selection
(gimp-selection-invert img)

; feather the selction
(gimp-selection-feather img feather)

; gaussian blur it
(plug-in-gauss-iir 1 img drawable amount amount amount)

; clear the visible elliptical selection
(gimp-selection-clear img)

)
; undo grouping finish
(gimp-image-undo-group-end img)
(gimp-displays-flush)
    )

(script-fu-register "script-fu-radial-fade-blur"
"<Image>/FX-Foundry/Artistic/Radial Blur..."
"Add a radially fading blur at a specified location."
"Gunther"
"gunther_oz@hotmail.com"
"2003"
"RGB*, GRAY*"
SF-IMAGE "Image" 0
SF-DRAWABLE "drawable" 0
SF-ADJUSTMENT _"Blur Amount" '(20 0 40 1 2 0 0)
SF-ADJUSTMENT _"Feather" '(20 0 100 1 2 0 0)
SF-ADJUSTMENT _"Exclusion Radius" '(20 0 8000 1 2 0 1)
SF-ADJUSTMENT _"Centre X" '(60 1 8000 1 10 0 1)
SF-ADJUSTMENT _"Centre Y" '(60 1 8000 1 10 0 1)
)

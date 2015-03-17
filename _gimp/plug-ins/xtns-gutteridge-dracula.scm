;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Dracula! script  for GIMP 2.4
; Chris Gutteridge (cjg@ecs.soton.ac.uk)
; At ECS Dept, University of Southampton, England.
;
; Tags: logo
;
; Author statement:
;I've got a new script, but I've not been able to publish it due it
;network faults + firewalls...
;
;I include it here, should you find a way to publish it for me.
;I appologise for how cheesy it is.
;
;(From the creator of fuzzy-selection and camo etc...)
;
; "Dracula!" or "Cheesy Goth"
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Updated to Gimp2.4 (11-2007) http://gimpscripts.com
; Added user selection options for all parameters
;
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


(define (script-fu-blood-logo inText inFont inFontSize inBorder inDrip)

    (define oldFore (car(gimp-palette-get-foreground)) )
    (define oldBack (car(gimp-palette-get-background)) )

    (define theImage (car(gimp-image-new 100 100 RGB)) )
    (define textLayer (car(gimp-layer-new theImage 10 10 RGBA-IMAGE "text layer" 100 NORMAL)) )
    (define bloodLayer (car(gimp-layer-new theImage 10 10 RGBA-IMAGE "blood layer" 100 NORMAL)) )
    (define backLayer(car(gimp-layer-new theImage 10 10 RGBA-IMAGE "Background" 100 NORMAL)) )
    (gimp-palette-set-background '(0 0 0) )
    (gimp-palette-set-foreground '(255 255 255))
    (gimp-image-add-layer theImage textLayer 0)
    (gimp-image-add-layer theImage bloodLayer 1)
    (gimp-image-add-layer theImage backLayer 2)
    (gimp-selection-all theImage)
    (gimp-edit-clear textLayer)
    (gimp-edit-clear bloodLayer)
    (gimp-selection-none theImage)
    (define theText (car (gimp-text theImage textLayer 0 0 inText 0 TRUE inFontSize PIXELS "*" inFont "*" "r" "*" "*" "*" "*")))
    (define theBuffer (+ inBorder inDrip inDrip))
    (gimp-layer-set-offsets theText theBuffer theBuffer)
    (define theWidth (+ (car(gimp-drawable-width theText)) theBuffer theBuffer) )
    (define theHeight (+ (car(gimp-drawable-height theText)) theBuffer theBuffer) )
    (gimp-image-resize theImage theWidth theHeight 0 0)
    (gimp-layer-resize textLayer theWidth theHeight 0 0)
    (gimp-layer-resize bloodLayer theWidth theHeight 0 0)
    (gimp-layer-resize backLayer theWidth theHeight 0 0)
    (gimp-floating-sel-anchor theText)

    (gimp-selection-all theImage)
    (gimp-edit-fill backLayer BACKGROUND-FILL)
    (gimp-selection-none theImage)

    (gimp-selection-layer-alpha textLayer)
    (gimp-selection-grow theImage inBorder)
    (gimp-edit-fill bloodLayer BACKGROUND-FILL)
    (gimp-selection-none theImage)

    (gimp-layer-scale bloodLayer theWidth (/ theHeight 8) TRUE)
    (define (smear n) (if (> n 0) (prog1 (plug-in-spread TRUE theImage bloodLayer 0 1) (smear (- n 1)))))
    (smear inDrip)
    (gimp-layer-scale bloodLayer theWidth theHeight TRUE)

    (gimp-selection-layer-alpha textLayer)
    (gimp-selection-grow theImage inBorder)
    (gimp-edit-fill bloodLayer BACKGROUND-FILL)
    (gimp-selection-none theImage)

    (plug-in-gauss-iir TRUE theImage bloodLayer 4 TRUE TRUE)
    (plug-in-threshold-alpha TRUE theImage bloodLayer 127)

    (gimp-selection-layer-alpha bloodLayer)
    (gimp-palette-set-foreground '(255 0 0))
    (gimp-bucket-fill bloodLayer FG-BUCKET-FILL 0 100 0 0 0 0)
    (gimp-selection-none theImage)
    (plug-in-gauss-iir TRUE theImage bloodLayer 1 TRUE TRUE)

    (define oneLayer (car(gimp-image-flatten theImage)) )
    (plug-in-autocrop TRUE theImage oneLayer)

    (gimp-palette-set-foreground oldFore)
    (gimp-palette-set-background oldBack)

    (gimp-display-new theImage)
)



; Register the function with the GIMP:

(script-fu-register
    "script-fu-blood-logo"
    "<Toolbox>/Xtns/FX-Foundry/Logos/Dracula"
    "Draws the given text string with a border of dripping blood!"
    "Christopher Gutteridge"
    "1998, Christopher Gutteridge"
    "2007, Scott Mosteller"
    ""
   SF-STRING     _"Text"               "Dracula"
   SF-FONT       _"Font"               "becker"
   SF-ADJUSTMENT _"Font size (pixels)" '(150 2 1000 1 10 0 1)
   SF-ADJUSTMENT _"Boder Size"    '(4 1 99 1 1 0 1)
   SF-ADJUSTMENT _"Drip Size"    '(10 5 99 1 1 0 1)
)


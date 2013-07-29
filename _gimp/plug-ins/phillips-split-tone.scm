;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Split Tone script  for GIMP 2.4
; Copyright (C) 2007 Harry Phillips <script-fu@tux.com.au>
;
; Tags: photo, split tone
;
; Author statement:
;
; This script converts an image to one that has one colour for highlights
; and one for shadows.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Changelog:
;  Version 1.7 (5th August 2007)
;    - Added GPL3 licence
;    - Menu location at the top of the script
;    - Removed the "script-fu-menu-register" section
;
;  Version 1.6
;    - Made the script compatible with GIMP 2.3
; --------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (my-duplicate-layer-at image layer pos)
    (let* ((dup-layer (car (gimp-layer-copy layer 1))))
              (gimp-image-add-layer image dup-layer pos)
          dup-layer))

(define (script-fu-split-tone     theImage
                theLayer
                highColour
                highOpacity
                shadColour
                shadOpacity
    )

    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    ;Initiate some variables
    (let* (
    (myForeground 0)
    (myBackground 0)
    (imageWidth 0)
    (imageHeight 0)
    (newLayer 0)
    (copy1 0)
    (copy2 0)
    (mergedShadows 0)
    (mergedHighlights 0)
    (mask 0)
    (layerRGB 0)
    )

    ;Read the current colours
    (set! myForeground (car (gimp-context-get-foreground)))
    (set! myBackground (car (gimp-context-get-background)))

    ;Select none
    (gimp-selection-none theImage)

    ;Detect if it is RGB
    (set! layerRGB (car (gimp-drawable-is-rgb theLayer)))

    ;Change the image RGB if it isn't already
    (if (= layerRGB 0) (gimp-image-convert-rgb theImage))

    ;Desaturate the layer
    (gimp-desaturate theLayer)

    ;Read the image height and width
    (set! imageWidth (car (gimp-image-width theImage)))
    (set! imageHeight (car (gimp-image-height theImage)))

    ;Set the foreground and background colours
    (gimp-context-set-foreground highColour)
    (gimp-context-set-background shadColour)

    ;Add the first layer to the image
    (set! copy1 (my-duplicate-layer-at theImage theLayer 0))
    ;(gimp-image-add-layer theImage copy1 0)

    ;Rename the layer
    (gimp-drawable-set-name copy1 "Shadows")

    ;Add a new layer
    (set! newLayer (car (gimp-layer-new theImage imageWidth imageHeight 0 "overlay" 100 5)))
    (gimp-image-add-layer theImage newLayer 0)

    ;Fill the layer with BG colour
    (gimp-edit-fill newLayer 1)

    ;Merge the layer down
    (set! mergedShadows (car (gimp-image-merge-down theImage newLayer 0)))

    ;Add a layer mask
    (set! mask (car (gimp-layer-create-mask mergedShadows 5)))
    (gimp-layer-add-mask mergedShadows mask)
    (gimp-invert mask)

    ;Change the merged layers opacity
    (gimp-layer-set-opacity mergedShadows shadOpacity)

    ;Add the second layer to the image
    (set! copy2 (my-duplicate-layer-at theImage theLayer 0))
    ;(gimp-image-add-layer theImage copy2 0)

    ;Rename the layer
    (gimp-drawable-set-name copy2 "Highlights")

    ;Add a new layer
    (set! newLayer (car (gimp-layer-new theImage imageWidth imageHeight 0 "overlay" 100 5)))
    (gimp-image-add-layer theImage newLayer 0)

    ;Fill the layer with FG colour
    (gimp-edit-fill newLayer 0)

    ;Merge the layer down
    (set! mergedHighlights (car (gimp-image-merge-down theImage newLayer 0)))

    ;Add a layer mask
    (set! mask (car (gimp-layer-create-mask mergedHighlights 5)))
    (gimp-layer-add-mask mergedHighlights mask)

    ;Change the merged layers opacity
    (gimp-layer-set-opacity mergedHighlights highOpacity)


    ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

    ;Set the FG and BG colours back to what they were
    (gimp-context-set-foreground myForeground)
    (gimp-context-set-background myBackground)

    ;Ensure the updated image is displayed now
    (gimp-displays-flush)


    )
)


(script-fu-register "script-fu-split-tone"
            _"<Image>/FX-Foundry/Color/Split Tone..."
            "Turns a B&W image into a split tone image"
            "Harry Phillips"
            "Harry Phillips"
            "Feb. 03 2006"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE     "Drawable"  0
            SF-COLOR "Highlight colour" '(255 144 0)
            SF-ADJUSTMENT   _"Highlight opacity:"     '(100 0 100 5 10 1 0)
            SF-COLOR "Shadows colour" '(0 204 255)
        SF-ADJUSTMENT   _"Shadow opacity:"      '(100 0 100 5 10 1 0)
)

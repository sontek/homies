;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Quick sketch script  for GIMP 2.4
; Copyright (C) 2007 Harry Phillips <script-fu@tux.com.au>
;
; Tags: photo, artistic
;
; Author statement:
;
; Quick sketch turns a photo into what looks like a artists sketch
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Last changed by author: 9 September 2007
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

(define (script-fu-quick-sketch     theImage
                    theLayer
                    blurAmount)

    ;Initiate some variables
    (let* (
    (layerCopy 0)
    (layerGrey (car (gimp-drawable-is-gray theLayer)))
    )

    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    ;Rename the layer
    (gimp-drawable-set-name theLayer "Original")

    ;Select none
    (gimp-selection-none theImage)

    ;Change the image Greyscale if it isn't already
    (if (= layerGrey 0) (gimp-image-convert-grayscale theImage))

    (set! layerCopy (car (gimp-layer-copy theLayer 1)))

    ;Copy the layer
    (gimp-image-add-layer theImage layerCopy 0)

    ;Rename the layer
    (gimp-drawable-set-name layerCopy "Dodge layer")

    ;Invert the layer
    (gimp-invert layerCopy)

    ;Change the layers mode
    (gimp-layer-set-mode layerCopy 16)

    ;Blur the dodge layer
    (plug-in-gauss 1 theImage layerCopy blurAmount blurAmount 0)

    ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

    ;Ensure the updated image is displayed now
    (gimp-displays-flush)


    )
)


(script-fu-register "script-fu-quick-sketch"
            _"<Image>/FX-Foundry/Artistic/Quick sketch..."
            "Create a sketch from a photo"
            "Harry Phillips"
            "Harry Phillips"
            "Sep. 9 2007"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE        "Drawable"  0
        SF-ADJUSTMENT    _"Blur factor"      '(30 5 200 1 1 0 0)
)

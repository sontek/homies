;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Double border script  for GIMP 2.4
; Copyright (C) 2007 Harry Phillips <script-fu@tux.com.au>
;
; Tags: decor, border
;
; Author statement:
;
; Creates two borders around you image with a shadow layer in between.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;  Version 1.3 (7th August 2007)
;    - Changed the order of the size checking so the error message is
;      at the top and makes easier reading.
;
;  Version 1.2 (5th August 2007)
;    - Added GPL3 licence
;    - Menu location at the top of the script
;    - Removed the "script-fu-menu-register" section
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


(define (layer-add-fill         width
                    height
                    label
                    opacity
                    colour
                    image)

    (let* ((newLayer (car (gimp-layer-new image width height 1 label opacity 0))))

    ;Set the background colour
    (gimp-context-set-background colour)

    ;Add the layer
    (gimp-image-add-layer image newLayer 100)

    ;Fill the shadow layer
    (gimp-drawable-fill newLayer 1)

    newLayer))

(define (script-fu-double-border    theImage
                    theLayer
                    innerColour
                    innerSize
                    outerColour
                    outerSize
                    shadowColour
                    shadowSize
                    shadowBlur
                    shadowOpacity
                    shadowInclude
    )

    ;Check that the outer size is larger than the required sie for the shadow
    (if (< outerSize (+ shadowSize shadowBlur))
    ;Outer size is too small
    (gimp-message "Outer size needs to larger than shadow size plus shadow blur")

    ;Outer size is large enough
    (begin


    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    (let*
    (
    ;Read the current colours
    (myForeground (car (gimp-context-get-foreground)))
    (myBackground (car (gimp-context-get-background)))

    ;Read the image width and height
    (imageWidth (car (gimp-image-width theImage)))
    (imageHeight (car (gimp-image-height theImage)))

    ;Calculate the size of the inner layer
    (innerWidth (+ imageWidth (* innerSize 2)))
    (innerHeight (+ imageHeight (* innerSize 2)))

    (shadBlur (+ shadowSize shadowBlur))
    (innerLayer)
    (outerLayer)
    (shadowLayer)

    (outerWidth (+ innerWidth (* outerSize 2)))
    (outerHeight (+ innerHeight (* outerSize 2)))
    (outerTemp)


    )

    ;Select none
    (gimp-selection-none theImage)

    ;Resize image
    (gimp-image-resize theImage innerWidth innerHeight innerSize innerSize)


    ;Add the inner layer to the image
    (set! innerLayer (layer-add-fill innerWidth innerHeight "Inner" 100 innerColour theImage))

    ;Add the shadow layer to the image
    (set! shadowLayer (layer-add-fill innerWidth innerHeight "Shadow" shadowOpacity shadowColour theImage))

    ;Check to see if the extra outer is wanted
    (if (= shadowInclude TRUE)
        (begin
            (set! outerWidth (+ outerWidth shadBlur))
            (set! outerHeight (+ outerHeight shadBlur))
        )
        ()
    )


    ;Resize image
    (gimp-image-resize theImage outerWidth outerHeight outerSize outerSize)

    ;Resize the shadow layer
    (gimp-layer-resize shadowLayer (+ innerWidth shadBlur) (+ innerHeight shadBlur) 0 0)

    ;Move the shadow
    (gimp-drawable-offset shadowLayer TRUE 0 shadowSize shadowSize)

    ;Blur the shadow layer
    (if (> shadowBlur 0)
    (plug-in-gauss 1 theImage shadowLayer shadowBlur shadowBlur 0)
    ()
    )

    ;Add the outer layer to the image
    (set! outerLayer (layer-add-fill outerWidth outerHeight "Shadow" 100 outerColour theImage))

    ;Reset the background colour
    (gimp-context-set-background myBackground)


    ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

    ;Ensure the updated image is displayed now
    (gimp-displays-flush)


))))


(script-fu-register "script-fu-double-border"
            _"<Image>/FX-Foundry/Image Effects/Double Border..."
            "Gives two borders with a drop shadow"
            "Harry Phillips"
            "Harry Phillips"
            "30 July 2007"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE        "Drawable"  0
            SF-COLOR        "Inner colour" '(255 255 255)
            SF-ADJUSTMENT    _"Inner size"     '(25 0 1024 1 10 1 0)
            SF-COLOR         "Outer colour" '(217 217 217)
        SF-ADJUSTMENT    _"Outer size"      '(50 0 1024 1 10 1 0)
            SF-COLOR         "Shadow colour" '(0 0 0)
        SF-ADJUSTMENT    _"Shadow size"         '(10 5 1024 1 10 0 1)
        SF-ADJUSTMENT    _"Shadow blur"    '(10 0 1024 1 10 0 1)
        SF-ADJUSTMENT    _"Shadow opacity"        '(80 0 100 1 10 0 0)
        SF-TOGGLE         _"Outer border is full width past shadow"       FALSE

)


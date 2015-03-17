;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Film Grain script  for GIMP 2.4
; Copyright (C) 2007 Harry Phillips <script-fu@tux.com.au>
;
; Tags: photo, grain, noise, film
;
; Author statement:
;
; Based on the tutorial  at http://www.gimpguru.org/Tutorials/FilmGrain/
; This script adds that gritty, art-house/street-photography/high-ISO
; grainy film look, especially in monochromatic photos.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Changelog:
;  Version 1.3 (5th August 2007)
;    - Added GPL3 licence
;    - Menu location at the top of the script
;    - Removed the "script-fu-menu-register" section
;
;  Version 1.2
;    - Made the script compatible with GIMP 2.3
;
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



(define (script-fu-film-grain    theImage
                theLayer
                grainSize
                blurAmount
                maskToggle
                midPoint)
 (define (mask-spline yPoint)
  (let* ((a (cons-array 6 'byte)))
    (set-pt a 0 0 0)
    (set-pt a 1 127 yPoint)
    (set-pt a 2 255 0)
    a))
 (define (set-pt a index x y)
    (prog1
    (aset a (* index 2) x)
    (aset a (+ (* index 2) 1) y)))

    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    (let*
    (
    (imageWidth 0)
    (imageHeight 0)
    (myForeground 0)
    (myBackground 0)
    (newLayer 0)
    (mask 0)
    )

    (set! imageWidth (car (gimp-image-width theImage)))
    (set! imageHeight (car (gimp-image-height theImage)))

    ;Read the current colours
    (set! myForeground (car (gimp-context-get-foreground)))
    (set! myBackground (car (gimp-context-get-background)))

    ;Select none
    (gimp-selection-none theImage)

    ;Set the foreground colour
    (gimp-context-set-foreground '(128 128 128))

    ;Add a new layer
    (set! newLayer (car (gimp-layer-new theImage imageWidth imageHeight 1 "Film Noise" 100 5)))
    (gimp-image-add-layer theImage newLayer 0)

    ;Fill the layer with BG colour
    (gimp-edit-fill newLayer 0)

    ;Open the scatter HSV
    (plug-in-scatter-hsv 1 theImage newLayer 2 3 10 grainSize)

    ;Apply the blur with the supplied blur amount
    (plug-in-gauss 1 theImage newLayer blurAmount blurAmount 0)


    (if (= maskToggle TRUE)
    (begin
        ;Add a layer mask
        (set! mask (car (gimp-layer-create-mask newLayer 0)))
        (gimp-layer-add-mask newLayer mask)

        ;Copy the layer into the clipboard
        (gimp-edit-copy theLayer)

        ;Paste it into the layer mask
        (gimp-floating-sel-anchor (car (gimp-edit-paste mask TRUE)))

        ;Alter the curves of the mask
        (gimp-curves-spline mask 0 6 (mask-spline midPoint))

    ) ())


    ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

    ;Set the FG and BG colours back to what they were
    (gimp-context-set-foreground myForeground)
    (gimp-context-set-background myBackground)

    ;Ensure the updated image is displayed now
    (gimp-displays-flush)

    )
)


(script-fu-register "script-fu-film-grain"
            _"<Image>/FX-Foundry/Photo/Effects/Film Grain..."
            "Performs a tone mapping operation with a specified blur on the open image"
            "Harry Phillips"
            "Harry Phillips"
            "May. 05 2007"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE     "Drawable"  0
            SF-ADJUSTMENT   _"Grain size"     '(50 10 500 5 5 1 0)
        SF-ADJUSTMENT   _"Blur amount"      '(1 1 5 0.1 1 1 0)
        SF-TOGGLE         _"Add a layer mask"       FALSE
            SF-ADJUSTMENT   _"Midpoint adjustment"     '(127 0 255 1 1 1 0)


)

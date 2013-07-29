;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Tonemapping script  for GIMP 2.4
; Copyright (C) 2007 Harry Phillips <script-fu@tux.com.au>;
; Author statement:
; Reduce global contrast while increasing local contrast and shadow/highlight
; detail.
;
; Tags: photo, tonemapng
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;  Version 1.3 (5th August 2007)
;    - Added GPL3 licence
;    - Menu location at the top of the script
;    - Removed the "script-fu-menu-register" section
;
;  Version 1.2
;    - Made the script compatible with GIMP 2.3
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


(define (my-duplicate-layer image layer)
    (let* ((dup-layer (car (gimp-layer-copy layer 1))))
              (gimp-image-add-layer image dup-layer 0)
          dup-layer))

(define (script-fu-tone-mapping theImage theLayer blurAmount opacityAmount)

    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    (let
    (
    (copy1 (my-duplicate-layer theImage theLayer))
    (copy2 (my-duplicate-layer theImage theLayer))
    )

    ;Apply the desaturate and invert to the top layer
    (gimp-desaturate copy2)
    (gimp-invert copy2)

    ;Apply the blur with the supplied blur amount
    (plug-in-gauss 1 theImage copy2 blurAmount blurAmount 0)

    ;Set the layers opacity
    (gimp-layer-set-opacity copy2 75)

    ;Merge the top layer down and keep track of the newly merged layer
    (let ((merged (car (gimp-image-merge-down theImage copy2 0))))

    ;Change the merged layers mode to SOFT LIGHT (19)
    (gimp-layer-set-mode merged 19)

    ;Change the merged layers opacity
    (gimp-layer-set-opacity merged opacityAmount))

    ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

    ;Ensure the updated image is displayed now
    (gimp-displays-flush)

    )
)


(script-fu-register "script-fu-tone-mapping"
            _"<Image>/FX-Foundry/Color/Tone Mapping"
            "Performs a tone mapping operation with a specified blur on the open image"
            "David Meiklejohn, Harry Phillips (Process)"
            "2006, David Meiklejohn, Harry Phillips (Process)"
            "Feb. 02 2006"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE     "Drawable"  0
            SF-ADJUSTMENT   _"Blur:"     '(100 100 500 10 10 1 0)
        SF-ADJUSTMENT   _"Opacity"      '(90 0 100 1 10 1 0)
)


;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; xMedia colorize script  for GIMP 2.4
; Original author: Alexander Melcher (a.melchers@planet.nl)
; At xMedia, The Netherlands
;
; Tags: photo, color
;
; Author statement:
;
; Applies a colour to a given selection or to the whole layer if no selection
; exists.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
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


; Define the function

(define (script-fu-colorize inImage
                        inLayer
                        inColor
                        inQuality
    )
   (let* (
          (coords 0)
         )
    ; Group undo information
    (gimp-undo-push-group-start inImage)
    (gimp-context-push)

    ; If run in normal mode, desaturate first
    (if (= inQuality 0)
        (gimp-desaturate inLayer)
        ()
    )

    ; Do the actual colorizing
    (gimp-palette-set-foreground inColor)
    (gimp-bucket-fill inLayer 0 13 100 255 FALSE 0 0)

    (set! coords (cdr (gimp-selection-bounds inImage)))
    (if (= (car (gimp-selection-is-empty inImage)) TRUE)
        (gimp-drawable-update inLayer 0 0
                              (car (gimp-drawable-width inLayer))
                              (car (gimp-drawable-height inLayer)))
        (gimp-drawable-update inLayer (car coords)
                                      (cadr coords)
                                      (caddr coords)
                                      (cadr (cddr coords)))
    )

    ; Group undo information
    (gimp-undo-push-group-end inImage)

    ; Force update
    (gimp-displays-flush)

    ; Return
    (list inImage inLayer)
    (gimp-context-push)
    )
)

; Register script-fu-colorize

(script-fu-register
    "script-fu-colorize"
    "<Image>/FX-Foundry/Color/xMedia Colorize..."
    "Applies a colour to a given selection or to the whole layer if no selection exists."
    "Alexander Melchers"
    "2002, Alexander Melchers, xMedia"
    "7th November 2002"
    "RGB*"
    SF-IMAGE    "The Image"           0
    SF-DRAWABLE "The Layer"           0
    SF-COLOR    "Color"               '(255 0 0)
    SF-OPTION   "Quality"             '("normal" "fastest")
)

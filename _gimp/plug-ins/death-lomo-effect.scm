;
;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; LOMO Effect script for GIMP 2.4
; Original author: Alexia Death
;
; Tags: photo, lomo
;
; Author statement:
;
; This represents my third attempt at gimp scripting and
; Scheme language. If you feel its not as good as it can be,
; feel free to improve it.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;TODO: FIX unblanked empty layer....
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


(define (script-fu-lomo-effect     inImage

                                      inLayer

                                      inCopy

                                      inFlatten

        )

  (let (

       (theWidth (car (gimp-image-width inImage)))

       (theHeight (car (gimp-image-height inImage)))

       (theImage 0)

       (base 0)

       (overlay 0)

       (floating-sel 0)

       )



    (set! theImage (if (= inCopy TRUE)

                     (car (gimp-image-duplicate inImage))

                     inImage

                   )

    )

    (if (= inCopy FALSE)

      (begin

        (gimp-image-undo-group-start theImage)

      )

    )

    (if (> (car (gimp-drawable-type inLayer)) 1)

        (gimp-image-convert-rgb theImage)

    )



; flattning the image at hand into a copy

    (gimp-edit-copy-visible theImage)

; Making base layer

    (set! base (car (gimp-layer-new theImage

                                        theWidth

                                        theHeight

                                        RGBA-IMAGE

                                        "base"

                                        100

                                        NORMAL-MODE)))

    (gimp-image-add-layer theImage base -1)



    (gimp-floating-sel-anchor (car (gimp-edit-paste base TRUE)))



    (gimp-hue-saturation base ALL-HUES 0 0 20)



    (gimp-brightness-contrast base 0 20)



; making vignetting layer

     (set! overlay (car (gimp-layer-new theImage

                                        theWidth

                                        theHeight

                                        RGBA-IMAGE

                                        "vignetting"

                                        100

                                        OVERLAY-MODE)))

    (gimp-image-add-layer theImage overlay -1)



    (gimp-context-push)



    (gimp-context-set-foreground '(255 255 255))



    (gimp-context-set-background '(0 0 0))



        (gimp-edit-blend overlay FG-BG-RGB-MODE NORMAL-MODE

                     GRADIENT-RADIAL 100 0 REPEAT-NONE FALSE

                     FALSE 0 0 TRUE

                     (/ theWidth 2) (/ theHeight 2) (- theWidth (/ theWidth 8)) (- theHeight (/ theHeight 8)))



    (gimp-context-pop)



    ;(gimp-layer-set-opacity overlay 50)



    (if (= inFlatten TRUE)

        (gimp-image-flatten theImage)

    )

    (if (= inCopy TRUE)

      (begin

        (gimp-image-clean-all theImage)

        (gimp-display-new theImage)

      )

    )

    (if (= inCopy FALSE)

      (begin

        (gimp-image-undo-group-end theImage)

      )

    )

    (gimp-displays-flush)

  )

)



(script-fu-register "script-fu-lomo-effect"

  _"_LOMO effect..."

  _"Give LOMO look to a photos."

  "Alexia Death"

  "2007, Alexia Death."

  "4rd October 2007"

  "RGB* GRAY*"

  SF-IMAGE      "The image"               0

  SF-DRAWABLE   "The layer"               0

  SF-TOGGLE     _"Work on copy"           FALSE

  SF-TOGGLE     _"Flatten image"          FALSE

)



(script-fu-menu-register "script-fu-lomo-effect"

                         "<Image>/FX-Foundry/Photo/Effects")


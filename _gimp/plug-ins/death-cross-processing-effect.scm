;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Cross Processing Effect script  for GIMP 2.4
; Original author: Alexia Death
;
; Tags: photo, crossprocessing
;
; Author statement:
;
; This represents my second attempt at gimp scripting and
; Scheme language. If you feel its not as good as it can be,
; feel free to improve it.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
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


(define (script-fu-cross-processing-effect     inImage

                                      inLayer

                                      inCopy

                                      inFlatten

                                      inColor

        )

  (let (

       (theWidth (car (gimp-image-width inImage)))
       (theHeight (car (gimp-image-height inImage)))
       (theImage 0)
       (base 0)
       (overlay 0)
       (colorcast 0)
       (floating-sel 0)
       (control_pts_r (cons-array 10 'byte))
       (control_pts_g (cons-array 8 'byte))
       (control_pts_b (cons-array 4 'byte))
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



    (set! control_pts_r #(0 0 88 47 170 188 221 249 255 255))

    (set! control_pts_g #(0 0 65 57 184 208 255 255))

    (set! control_pts_b #(0 29 255 226))





    (gimp-curves-spline base HISTOGRAM-RED 10 control_pts_r)

    (gimp-curves-spline base HISTOGRAM-GREEN 8 control_pts_g)

    (gimp-curves-spline base HISTOGRAM-BLUE 4 control_pts_b)



; making overlay layer

     (set! overlay (car (gimp-layer-new theImage

                                        theWidth

                                        theHeight

                                        RGBA-IMAGE

                                        "overlay"

                                        100

                                        OVERLAY-MODE)))

    (gimp-image-add-layer theImage overlay -1)



    (gimp-floating-sel-anchor (car (gimp-edit-paste overlay TRUE)))



    (gimp-layer-set-opacity overlay 50)



; making colorcast layer

     (set! colorcast (car (gimp-layer-new theImage

                                        theWidth

                                        theHeight

                                        RGBA-IMAGE

                                        "colorcast"

                                        100

                                        OVERLAY-MODE)))

    (gimp-image-add-layer theImage colorcast -1)

    (gimp-context-push)

    (gimp-context-set-foreground inColor)

    (gimp-drawable-fill colorcast FOREGROUND-FILL)

    (gimp-context-pop)

    (gimp-layer-set-opacity colorcast 10)







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







(script-fu-register "script-fu-cross-processing-effect"

  _"_Cross processing effect..."

  _"Give cross processing look to a photo."

  "Alexia Death"

  "2007, Alexia Death."

  "3rd October 2007"

  "RGB* GRAY*"

  SF-IMAGE      "The image"               0

  SF-DRAWABLE   "The layer"               0

  SF-TOGGLE     _"Work on copy"           FALSE

  SF-TOGGLE     _"Flatten image"          FALSE

  SF-COLOR     _"Overcast color"          '(0 255 180)

)



(script-fu-menu-register "script-fu-cross-processing-effect"

                         "<Image>/FX-Foundry/Photo/Effects")


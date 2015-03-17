;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
; 
; Match colors script  for GIMP 2.4
; Copyright (C) 2007 Kevin Cozens
;
; Tags: colors
;
; Author statement:
;
;This script matches the colours in one single layer image to the colours in
;another image. It is an implementation of the method described by xooorx at:
;http://www.flickr.com/groups/gimpusers/discuss/72157600180607090/
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


(define (script-fu-match-colors img drawable master-img)
  (let* (
        (width (car (gimp-drawable-width drawable)))
        (height (car (gimp-drawable-height drawable)))
        (master-img (car (gimp-image-duplicate master-img)))
        (layer-copy 0)
        (decomposed-new 0)
        (decomposed-old 0)
        (layers 0)
        )

    ;Add checks to make sure each image only has a single layer.

    ;Step 1
    (gimp-image-scale master-img width height)

    ;Step 2
    (gimp-image-convert-indexed master-img NO-DITHER MAKE-PALETTE 256
                                                     FALSE TRUE "")

    ;Step 3
    ;Check that copy returned true
    (set! layer-copy (car (gimp-layer-new-from-drawable drawable master-img)))
    (gimp-image-add-layer master-img layer-copy -1)

    ;Step 4
    (gimp-image-convert-rgb master-img)
    (set! layer-copy (car (gimp-image-flatten master-img)))
    (set! decomposed-new (car (plug-in-decompose RUN-NONINTERACTIVE
                                                 master-img layer-copy
                                                 "YCbCr_ITU_R470" TRUE)))

    (gimp-image-delete master-img)  ;We are done with this copy

    ;Step 5
    (set! drawable (car (gimp-image-flatten img)))
    (set! decomposed-old (car (plug-in-decompose RUN-NONINTERACTIVE
                                                 img drawable
                                                 "YCbCr_ITU_R470" TRUE)))

    ;Step 6
    (set! layers (cadr (gimp-image-get-layers decomposed-old)))
    (gimp-image-remove-layer decomposed-old (aref layers 1))
    (gimp-image-remove-layer decomposed-old (aref layers 2))

    ;Step 7 and 8
    (set! layers (cadr (gimp-image-get-layers decomposed-new)))
    (set! layer-copy (car (gimp-layer-new-from-drawable (aref layers 1)
                                                        decomposed-old)))
    (gimp-image-add-layer decomposed-old layer-copy 1)
    (plug-in-blur RUN-NONINTERACTIVE decomposed-old layer-copy)

    (set! layer-copy (car (gimp-layer-new-from-drawable (aref layers 2)
                                                         decomposed-old)))
    (gimp-image-add-layer decomposed-old layer-copy 2)
    (plug-in-blur RUN-NONINTERACTIVE decomposed-old layer-copy)

    (gimp-image-delete decomposed-new)  ;We are done with this image

    ;Step 9
    (set! layers (cadr (gimp-image-get-layers decomposed-old)))
    (set! decomposed-new (car (plug-in-drawable-compose RUN-NONINTERACTIVE
                                                        decomposed-old
                                                        (aref layers 0)
                                                        (aref layers 1)
                                                        (aref layers 2)
                                                        -1
                                                        "YCbCr_ITU_R470"
                                                         )))

    (gimp-image-delete decomposed-old)  ;We are done with this image

    (gimp-display-new decomposed-new)
  )
)

(script-fu-register "script-fu-match-colors"
 _"_Match Colors..."
 _"Match the colors of a single layer image to the colors of another single layer image."
	"Kevin Cozens <kevin@ve3syb.ca>"
	"Kevin Cozens"
	"September 12, 2007"
	"RGB*"
	SF-IMAGE    _"Image"          0
	SF-DRAWABLE _"Drawable"       0
	SF-IMAGE    _"Image to match" 0
)

(script-fu-menu-register "script-fu-match-colors"
			                  _"<Image>/FX-Foundry/Color/")


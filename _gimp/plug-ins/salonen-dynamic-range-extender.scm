;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Dynamic Range Extender script  for GIMP 2.4
; Original author: Olli Salonen <olli@cabbala.net>
;
; Tags: photo, exposure, fake hdr
;
; Author statement:
;
; script-fu-dynamic-range-extender - A script that will produce an image
; with extended dynamic range. The input is an image with 2 layers, lighter
; (eg. exposed for sky) MUST be the lower layer and darker (eg. exposed for
; shadows) MUST be the upper layer. Please note that the upper layer must
; be anchored, floating selection is not sufficent.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Olli Salonen reated version 1.00  on Jan 06, 2004
; Revised 10/26/2007 by Mark Lowry to fix unbound variables
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

(define (script-fu-dynamic-range-extender inImg inDrw inBlurAmount inOpacity inMerge)

  (let* (
      (layers 0)
      (top-layer 0)
      (bottom-layer 0)
      (top-layer-mask 0)
      (bottom-layer-copy 0)
      )

    (set! layers (cadr (gimp-image-get-layers inImg)))
    (set! top-layer (vector-ref layers 0))
    (set! bottom-layer (vector-ref layers 1))
    (set! top-layer-mask (car (gimp-layer-create-mask top-layer 0)))
    (set! bottom-layer-copy (car (gimp-layer-copy bottom-layer FALSE)))

    (gimp-image-undo-group-start inImg)

    (gimp-image-add-layer-mask inImg top-layer top-layer-mask)

    (gimp-image-set-active-layer inImg bottom-layer)
    (gimp-selection-all inImg)
    (gimp-edit-copy bottom-layer)
    (gimp-selection-none inImg)
    (gimp-floating-sel-anchor (car (gimp-edit-paste top-layer-mask FALSE)))
    (plug-in-gauss-iir 1 inImg top-layer-mask inBlurAmount TRUE TRUE)
    (gimp-layer-set-opacity top-layer inOpacity)
    (if (= inMerge TRUE)
    (gimp-image-merge-down inImg top-layer 2)
    )

    (gimp-image-undo-group-end inImg)
    (gimp-displays-flush)

    )
)

(script-fu-register "script-fu-dynamic-range-extender"
            "<Image>/FX-Foundry/Photo/Enhancement/_Dynamic Range Extender"
            "Blend two differently exposed images together thus increasing dynamic range. Image must contain 2 layers - lighter (eg. exposed for shadows) MUST be the lower layer and darker (eg. exposed for sky) MUST be the upper layer. Please note that the upper layer must be anchored, floating selection is not sufficent. \n\nBlur strength controls the amount of gaussian blue applied to the mask. Different values are suitable for images with variable amount of details. Opacity is the opacity of the darker layer with the mask, usually 100 is just fine."
            "Olli Salonen <olli@cabbala.net>"
            "Olli Salonen"
            "Jan 06, 2004"
            "RGB* GRAY*"
            SF-IMAGE              "Image"                0
            SF-DRAWABLE           "Drawable"             0
            SF-ADJUSTMENT         "Blur strength"        '(15 0 50 1 1 0 0)
            SF-ADJUSTMENT         "Opacity"              '(100 0 100 1 1 0 0)
            SF-TOGGLE             "Merge layers (Dark on top, Light on bottom)"         TRUE)



;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Hall Smart Sharpen script  for GIMP 2.4
; Copyright (c) 2004-2005 John Hall (jhall@gimpfaq.org)
; Modified 2006/07/09 Ade Rixon (ade.rixon@NOTTHISBIT.bigfoot.com,
;   http://www.big-bubbles.fluff.org)
;
; Tags: photo, sharpen
;
; Author statement:
;
;;; A Script-Fu script for The GIMP to automate "smart" sharpening
;;; technique.
;;;
;;
;; This technique is described by Eric R. Jeschke here:
;; http://gimpguru.org/Tutorials/SmartSharpening2/
;;
;; However, he adapted this technique for The GIMP from a sharpening
;; article by Bruce Fraser on the creativepro.com web site:
;; http://www.creativepro.com/printerfriendly/story/20357.html
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;
;   v0.3b: - Handle grayscale images; ajr
;   v0.3a: - Change USM & edge-detect params via dialog; ajr
;   v0.3: - Cleaned up code
;   v0.2: - Modified for The GIMP 2.0pre2
;   v0.1: - First public release
; Updated for GIMP 2.4 by Paul Sherman
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



(define (script-fu-edge-mask-sharpen image drawable inAmount inRadius inDespeckle)

  ;; Uses a logarithmic scale to calculate a reasonable amount for
  ;; edge detection.
  (define (get-edge-amount width height)
    (min (max (* 3.5 (log (/ (* width height) 1000000)))
              1.0)
         10.0))

  (let* ((width (car (gimp-image-width image)))
     (height (car (gimp-image-height image)))
     (edge-amount (get-edge-amount width height))
         (blur-radius (+ 1 edge-amount))
     (edge-image (car (gimp-image-duplicate image)))
         (edge-drawable (car (gimp-image-flatten edge-image)))
         ;;(image-type (car (gimp-image-base-type edge-image)))
     (value-image (cond ((= (car (gimp-image-base-type image)) RGB)
         ;; decompose to value part if RGB image
            (caddr (plug-in-decompose TRUE
                                        edge-image
                                        edge-drawable
                                        "HSV"
                                        FALSE))
        )
            ;; else just copy if gray
         (t
            (car (gimp-image-duplicate edge-image))
        )
     ))
         (value-drawable (car (gimp-image-get-active-layer value-image)))
         (sharpen-layer (car (gimp-layer-new image
                                             (car (gimp-image-width image))
                                             (car (gimp-image-height image))
                                             (car (gimp-drawable-type-with-alpha drawable))
                                             "Sharpening"
                                             80.0
                                             VALUE-MODE)))
         (sharpen-layer-mask (car (gimp-layer-create-mask sharpen-layer
                                                          WHITE-MASK))))

    (gimp-image-undo-group-start image)

    ;; Apply an unsharp mask to the value image
    (plug-in-unsharp-mask TRUE value-image value-drawable inRadius inAmount 0)

    ;; Copy value image to sharpen layer
    (gimp-image-add-layer image sharpen-layer -1)
    (gimp-edit-copy value-drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste sharpen-layer TRUE)))

    ;; Despeckle edge image to remove noise (grain)
    (if (= inDespeckle TRUE)
    (plug-in-despeckle TRUE edge-image edge-drawable 3 1 3 248)
    )

    ;; Adjust the tonality so that areas that need sharpening are
    ;; white and everything else is black (this will become the
    ;; sharpening mask).  The input levels were picked arbitrarily,
    ;; but they seem to work for me.
    (plug-in-edge TRUE edge-image edge-drawable edge-amount 1 0)
    (if (= (car (gimp-image-base-type image)) RGB)
        (gimp-image-convert-grayscale edge-image))
    (gimp-levels edge-drawable VALUE-LUT 40 200 0.90 0 255)
    (plug-in-gauss-iir2 TRUE edge-image edge-drawable blur-radius blur-radius)
    (gimp-levels-auto edge-drawable)

    ;; Copy edge image to sharpen layer mask
    (gimp-layer-add-mask sharpen-layer sharpen-layer-mask)
    (gimp-edit-copy edge-drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste sharpen-layer-mask TRUE)))
    (gimp-image-delete edge-image)

    (gimp-image-undo-group-end image)
    (gimp-displays-flush)

    (list image)))

(script-fu-register "script-fu-edge-mask-sharpen"
                    "<Image>/FX-Foundry/Photo/Sharpen/Edge Mask Sharpen"
                    "Perform a smart sharpen on an image."
                    "John Hall"
                    "John Hall"
                    "2004-2005"
                    "RGB* GRAY*"
                    SF-IMAGE "The image" 0
                    SF-DRAWABLE "The layer (not used)" 0
            SF-ADJUSTMENT "Amount of USM" '(1.5 0 10 0.01 0.01 2 0)
            SF-ADJUSTMENT "Radius of USM" '(1.0 0 20 0.01 0.01 2 0)
            SF-TOGGLE "Despeckle" FALSE
            )

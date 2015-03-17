;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Smart sharpening script  for GIMP 2.4
; Original author: Olli Salonen <olli@cabbala.net>
;
; Tags: photo, sharpen
;
; Author statement:
;
; script-fu-smart-sharpening - Smart sharpening of image. This script finds
; the edges of images and only sharpens those.
;
; You can find more about smart sharpening at
; http://www.gimpguru.org/Tutorials/SmartSharpening/
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Changelog:
; 1.00 - Jan 07, 2004 initial release
; 1.01 - 10/31/2007 - upgrade for Gimp 2.4 by Alexia Death
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


(define (script-fu-lab-sharpening inImg inDrw inAmount inRadius inEdge)

  (let* (
     (original inImg)
     (template (car (gimp-image-duplicate original)))
     (original-layers (cadr (gimp-image-get-layers inImg)))
     (template-layers (cadr (gimp-image-get-layers template)))
     (template-bg-copy (car (gimp-layer-copy (aref template-layers 0) TRUE)))
     (width (car (gimp-image-width original)))
     (height (car (gimp-image-height original)))
     (sharpen-mask 0)
     (lab-image 0)
     (lab-layers 0)
     (final-mask 0)
     (result-image 0)
     (result-layers 0)
     )

    (define (spline)
      (let* ((a (cons-array 8 'byte)))
    (set-pt a 0 0 0)
    (set-pt a 1 166 0)
    (set-pt a 2 246 255)
    (set-pt a 3 255 255)
    a))

    (define (set-pt a index x y)
    (prog1
    (aset a (* index 2) x)
    (aset a (+ (* index 2) 1) y)))

    (gimp-image-undo-group-start inImg)

    (gimp-image-add-layer template template-bg-copy -1)
    (gimp-image-set-active-layer template template-bg-copy)
    (gimp-selection-all template)
    (gimp-edit-copy template-bg-copy)
    (set! sharpen-mask (car (gimp-channel-new template width height "SharpenMask" 50 '(255 0 0))))
    (gimp-image-add-channel template sharpen-mask 0)
    (gimp-floating-sel-anchor (car (gimp-edit-paste sharpen-mask FALSE)))
    (plug-in-edge TRUE template sharpen-mask inEdge 1 0)
    (gimp-invert sharpen-mask)
    (gimp-curves-spline sharpen-mask 0 8 (spline))
    (plug-in-gauss-iir TRUE template sharpen-mask 1 TRUE TRUE)
    (gimp-edit-copy sharpen-mask)

    ; split to L*a*b* and sharpen only L-channel
    (set! lab-image (car (plug-in-decompose TRUE original (aref original-layers 0) "LAB" TRUE)))
    (set! lab-layers (cadr (gimp-image-get-layers lab-image)))
    (set! final-mask (car (gimp-channel-new lab-image width height "FinalMask" 50 '(255 0 0))))
    (gimp-image-add-channel lab-image final-mask 0)
    (gimp-floating-sel-anchor (car (gimp-edit-paste final-mask FALSE)))
    (gimp-image-delete template)
    (gimp-selection-load final-mask)
    (gimp-selection-invert lab-image)
    (gimp-selection-shrink lab-image 1)
    (gimp-image-remove-channel lab-image final-mask)
    (plug-in-unsharp-mask TRUE lab-image (aref lab-layers 0) inRadius inAmount 0)
    (gimp-selection-none lab-image)

    ; compose image from Lab-channels
    (set! result-image (car (plug-in-drawable-compose TRUE 0 (aref lab-layers 0) (aref lab-layers 1) (aref lab-layers 2) 0 "LAB")))
    (set! result-layers (cadr (gimp-image-get-layers result-image)))
    (gimp-edit-copy (aref result-layers 0))
    (gimp-image-delete lab-image)
    (gimp-image-delete result-image)


;    (set! sharpened (car (gimp-layer-new original width height 1 "Result" 100 0)))
;    (gimp-image-add-layer original sharpened -1)

    (gimp-floating-sel-anchor (car (gimp-edit-paste (aref original-layers 0) FALSE)))


    (gimp-image-undo-group-end inImg)
    (gimp-displays-flush)
    )
)

(script-fu-register "script-fu-lab-sharpening"
            "<Image>/FX-Foundry/Photo/Sharpen/Luminocity Sharpen"
            "Sharpen images intelligently. Smart sharpen only sharpens images on the edges, where sharpening counts. Even areas are not sharpened, so noise levels are kept down when compared to normal unsharp mask. You may need to tweak the parameters for best result."
            "Olli Salonen <olli@cabbala.net>"
            "Olli Salonen"
            "Jan 07, 2004"
            ""
            SF-IMAGE              "Image"                0
            SF-DRAWABLE           "Drawable"             0
            SF-ADJUSTMENT         "Amount of USM"        '(0.5 0 10 0.01 0.01 2 0)
            SF-ADJUSTMENT         "Radius of USM"        '(0.5 0 10 0.01 0.01 2 0)
            SF-ADJUSTMENT         "FindEdge amount"      '(2.0 0 10 0.01 0.01 2 0)

            )



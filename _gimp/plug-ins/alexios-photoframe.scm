;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Photo Frame script  for GIMP 2.4
; Original author: Alexios Chouchoulas
;
; Tags: decor, frame
;
; Author statement:
;
; This is a rather simple script to produce a photographic frame
; around an image. This resembles a simplistic full-frame print: a
; thin black frame outlines the photograph itself, with thicker white
; borders around frame and picture. The colours and thicknesses are,
; of course, customisable.
;

; Written on top of Chris Gutteridge's (cjg@ecs.soton.ac.uk) Fuzzy
; Border script (which was only used as a template, but there you go).
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Revision 2.1  2005/01/02 20:22:22  alexios
; Fixed stupid sanity check bug that manifests in GIMP 2.2.
;
; Revision 2.0  2004/09/14 20:45:25  alexios
; Stepped version to recover CVS repository after near-catastrophic disk
; crash.
;
; Revision 1.3  2004/02/01 14:07:15  alexios
; One simple (but show-stopping) bug fix.
;
; Revision 1.2  2004/01/31 17:00:21  alexios
; Added support for Indexed images (they're converted to RGB anyway).
;
; Revision 1.1  2004/01/31 16:52:52  alexios
; Initial branched revision. Fixes a couple of small issues.
;
; Revision 1.1  2004/01/31 12:38:10  alexios
; Manually branched initial revision
;
; Revision 1.2  2003/11/21 11:26:31  alexios
; Backported to GIMP 1.2.
;
; Revision 1.1.1.1  2003/09/20 15:33:06  alexios
; Initial revision.
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

; Define the function:

(define (alexios-draw-frame inImage inFrameWidth inColour inLayerName)
  (let* (
          (theWidth 0)
          (theHeight 0)
          (theLayer 0)
        )
  (set! theWidth (car (gimp-image-width inImage)))
  (set! theHeight (car (gimp-image-height inImage)))

  (gimp-image-resize inImage
             (+ theWidth (* inFrameWidth 2))
             (+ theHeight (* inFrameWidth 2))
             inFrameWidth
             inFrameWidth)

  (gimp-selection-all inImage)
  (set! theWidth (car (gimp-image-width inImage)))
  (set! theHeight (car (gimp-image-height inImage)))

  (set! theLayer (car (gimp-layer-new     inImage
                    theWidth
                    theHeight
                    RGBA-IMAGE
                    inLayerName
                    100
                    NORMAL-MODE)))

  (gimp-image-add-layer inImage theLayer 0)
  (gimp-palette-set-background inColour)
  (gimp-edit-clear theLayer)
  (gimp-edit-fill theLayer BG-IMAGE-FILL)
  (gimp-image-lower-layer-to-bottom inImage theLayer)

;  inImage
  )
)


(define (script-fu-photo-frame    inImage
                inLayer
                inFrameColour
                inFrameWidth
                inPaddingColour
                inPaddingWidth
                inCopy
        inFlatten
    )
  (let* (
          (theImage 0)
          (theWidth 0)
          (theHeight 0)
          (mode 0)
        )
        (gimp-selection-all inImage)
        (set! theImage (if (= inCopy TRUE)
                   (car (gimp-image-duplicate inImage))
                           inImage)
            )

        (if (< 0 (car (gimp-image-base-type theImage)))
            (gimp-image-convert-rgb theImage))

        ;(set! mode (car (gimp-drawable-type-with-alpha inLayer)))
        (set! mode 'RGBA-IMAGE)

            (set! theWidth (car (gimp-image-width theImage)))
        (set! theHeight (car (gimp-image-height theImage)))

        ; Add an alpha channel to the bottom layer.

        (let* (
               (layers (gimp-image-get-layers theImage))
               (num-layers (car layers))
               (layer-array (cadr layers)))

          (gimp-layer-add-alpha (aref layer-array (- num-layers 1))))

        ; Draw the frame.

        (alexios-draw-frame theImage inFrameWidth inFrameColour "Frame" mode)

        ; Draw the padding.

        (alexios-draw-frame theImage inPaddingWidth inPaddingColour "Padding" mode)

        ; Flatten the image, if we need to.

        (if (= inFlatten TRUE) (gimp-image-flatten theImage) ())

        ; Have we been working on a copy? If so display the new image.

        (if (= inCopy TRUE)
            (begin
              (gimp-image-clean-all theImage)
              (gimp-display-new theImage)
              )
            ()
        )

        ; The end.

        (gimp-displays-flush)
    )
)

; Register the function with the GIMP:

(script-fu-register "script-fu-photo-frame"
    _"<Image>/FX-Foundry/Image Effects/Alexios Photo Frame..."
    "Frame a photograph"
    "Alexios Chouchoulas"
    "2003, Alexios Chouchoulas"
    "20th September 2003"
    "RGB* GRAY* INDEXED*"
    SF-IMAGE       "The Image"      0
    SF-DRAWABLE    "The Layer"      0
    SF-COLOR       _"Frame color"   '(0 0 0)
    SF-ADJUSTMENT  _"Frame width"   '(3 1 300 1 10 0 1)
    SF-COLOR       _"Padding color" '(255 255 255)
    SF-ADJUSTMENT  _"Padding width" '(10 1 300 1 10 0 1)
    SF-TOGGLE      _"Work on Copy"  TRUE
    SF-TOGGLE      _"Flatten Image" TRUE
)

;;; End Of File.

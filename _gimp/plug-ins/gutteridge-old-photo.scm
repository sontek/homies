;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Old Photo script  for GIMP 2.4
; Original author: Chris Gutteridge (cjg@ecs.soton.ac.uk)
; At ECS Dept, University of Southampton, England.
;
; Tags: photo, vintage, old
;
; Author statement:
; Special thanks to Daniele Medri (madrid @ linux . it)
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Davide Galimberti (dgali@tiscalinet.it)
; 16-jan-2001 At Torino, Italy.
;
; Added 'Border Size' adjustment
; Added 'Mottle Size' adjustment
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

(define (script-fu-old-photo-2 inImage inLayer inDefocus inBorder BorderSize inSepia inMottle MottleSize inCopy)
  (let* (
          (theImage 0)
          (theLayer 0)
          (theWidth 0)
          (theHeight 0)
          (mLayer 0)
        )
        (gimp-selection-all inImage)
        (set! theImage (if (= inCopy TRUE)
                       (car (gimp-image-duplicate inImage))
                       inImage)
        )

    (set! theLayer (car(gimp-image-flatten theImage)))
        (if (= inDefocus TRUE)
            (plug-in-gauss-rle TRUE theImage theLayer 1.5 TRUE TRUE)
            ()
        )
        (if (= inBorder TRUE)
            (script-fu-fuzzy-border theImage inLayer '(255 255 255)
                BorderSize TRUE 8 FALSE 100 FALSE TRUE )
            ()
        )
    (set! theLayer (car(gimp-image-flatten theImage)))

    (if (= inSepia TRUE)
            (begin (gimp-desaturate theLayer)
               (gimp-brightness-contrast theLayer -20 -40)
               (gimp-color-balance theLayer 0 TRUE 30 0 -30)
            )
            ()
        )
        (set! theWidth (car (gimp-image-width theImage)))
        (set! theHeight (car (gimp-image-height theImage)))
    (if (= inMottle TRUE)
            (begin (set! mLayer (car (gimp-layer-new theImage theWidth theHeight RGBA-IMAGE "Mottle" 100 DARKEN-ONLY-MODE)))

                   (gimp-image-add-layer theImage mLayer 0)
                   (gimp-selection-all theImage)
                   (gimp-edit-clear mLayer)
                   (gimp-selection-none theImage)
                   (plug-in-noisify TRUE theImage mLayer TRUE 0 0 0 MottleSize)
                   (plug-in-gauss-rle TRUE theImage mLayer 5 TRUE TRUE)
               (set! theLayer (car(gimp-image-flatten theImage)))
            )
            ()
        )



        (if     (= inCopy TRUE)
                (begin  (gimp-image-clean-all theImage)
                        (gimp-display-new theImage)
                )
                ()
        )
        (gimp-selection-none inImage)
    (gimp-displays-flush theImage)
 )
)

; Register the function with the GIMP:

(script-fu-register
    "script-fu-old-photo-2"
    "<Image>/FX-Foundry/Photo/Effects/Old Photo..."
    "Makes the image look like an old photo (more configurable)"
    "Chris Gutteridge - Modification by Davide Galimberti (dgali@tiscalinet.it)"
    "1998, Chris Gutteridge / ECS dept, University of Southampton, England. - 2001, Davide Galimberti (dgali@tiscalinet.it)"
    "16th April 1998"
    "RGB* GRAY*"
    SF-IMAGE "The Image" 0
    SF-DRAWABLE "The Layer" 0
    SF-TOGGLE "Defocus" TRUE
    SF-TOGGLE "Border" TRUE
    SF-ADJUSTMENT "Border Size" '(20 1 200 1 10 0 1)
    SF-TOGGLE "Sepia" TRUE
    SF-TOGGLE "Mottle" FALSE
    SF-ADJUSTMENT "Mottle Size" '(0.5 0 10 0.1 10 2 1)
    SF-TOGGLE "Work on Copy" TRUE
)

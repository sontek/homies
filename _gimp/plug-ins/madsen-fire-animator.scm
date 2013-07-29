;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Fire animator script  for GIMP 2.4
; Created by Vidar Madsen <vidar@prosalg.no>
; Contributions, moral and "codal"(?) support:
; - Guillaume <G.DeSercey@bton.ac.uk>
; - Patrick Hohmeyer <P.Hohmeyer@web.de>
; - Jim Miller <magnan@xinu.nu>
; - Mario Cosenza <mcosenza@engsvcs.com>
;
; Tags: fire, alpha, logo, animation
;
; Author statement:
;
; Basically, it takes an image (with an alpha-channel), and sets
; it on fire. ;-) The user can specify how many frames should be
; used, and which gradient to use to color the flames. The defaults
; ought to work nicely most of the time, though.
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

(define (script-fu-fireanim-doit image drawable mask xoffs yoffs usebackgnd)
  (plug-in-spread 1 image drawable 1.0 1.0)
  (gimp-levels drawable HISTOGRAM-VALUE 1 255 1.0 0 242)
  (gimp-selection-layer-alpha mask)
  (plug-in-noisify RUN-NONINTERACTIVE image drawable FALSE 2.0 2.0 2.0 0.0)
  (gimp-selection-grow image 1)
  (plug-in-noisify RUN-NONINTERACTIVE image drawable FALSE 0.3 0.3 0.3 0.0)
  (gimp-selection-grow image 2)
  (plug-in-gauss-rle TRUE image drawable 1.0 TRUE TRUE)
  (gimp-selection-none image)
  (gimp-context-set-background '(0 0 0))
  (gimp-drawable-offset drawable FALSE usebackgnd xoffs yoffs)
  (gimp-selection-layer-alpha mask)
  (gimp-context-set-background '(255 255 255))
  (gimp-edit-fill drawable BACKGROUND-FILL)
  (gimp-selection-none image)
  )

(define (script-fu-fireanim-prep image drawable orig gradient)
  (gimp-levels drawable HISTOGRAM-VALUE 1 200 1.0 0 255)
  (gimp-layer-set-lock-alpha drawable TRUE)
  (plug-in-gradmap TRUE image drawable)
  (gimp-layer-set-lock-alpha drawable FALSE)
  (gimp-selection-layer-alpha orig)
  (gimp-edit-copy orig)
  (gimp-floating-sel-anchor (car (gimp-edit-paste drawable FALSE)))
  (gimp-selection-none image)
  )

(define (script-fu-fireanim-copylayer dstimage dstdrawable srcimage srcdrawable)
  (gimp-selection-all dstimage)
  (gimp-edit-clear dstdrawable)
  (gimp-selection-none dstimage)
  (gimp-selection-all srcimage)
  (gimp-edit-copy srcdrawable)
  (gimp-floating-sel-anchor (car (gimp-edit-paste dstdrawable FALSE)))
  )

(define (script-fu-fireanim origimage origdrawable nframes oframes pframes
                            gradient framerate angle distance usebackgnd backgnd)
 (let* (
     (width 0)
     (height 0)
     (xoffs 0)
     (yoffs 0)
     (image 0)
     (drawable 0)
     (numlayers 0)
     (layerlist 0)
     (count 0)
     (bgimage 0)
     (bgcopy 0)
     (frame 0)
     (framea 0)
     (frameb 0)
     (larray 0)
     (newlayer 0)
     (opac 0)
     (bglayer 0)
  )
  (if (< nframes oframes)
    (error "Number of frames < overlap-frames!"))
  (gimp-context-set-gradient gradient)
  (set! width (car (gimp-image-width origimage)))
  (set! height (car (gimp-image-height origimage)))
  (set! xoffs (* distance (sin (* angle (/ 3.141592654 180)))))
  (set! yoffs (- 0 (* distance (cos (* angle (/ 3.141592654 180))))))
  (set! image (car (gimp-image-duplicate origimage)))
  (set! drawable (car (gimp-image-get-active-layer image)))

  (gimp-image-undo-disable image)
  (set! numlayers (car (gimp-image-get-layers image)))
  (set! layerlist (cadr (gimp-image-get-layers image)))
  (set! count numlayers)
  (while (> count 0)
    (set! count (- count 1))
    (if (not (= (aref layerlist count) drawable))
      (gimp-image-remove-layer image (aref layerlist count))
      )
    )
  (if (= usebackgnd TRUE)
    (begin
      (set! bgimage (car (gimp-drawable-get-image backgnd)))
      (set! bgcopy (car (gimp-layer-new image width height RGBA-IMAGE "bgcopy" 100 NORMAL)))
      (gimp-image-add-layer image bgcopy 0)
      (script-fu-fireanim-copylayer image bgcopy bgimage backgnd)
      (gimp-drawable-set-visible bgcopy FALSE)
      )
    )
  (set! frame (car (gimp-layer-new image width height RGBA-IMAGE "frame" 100 NORMAL)))
  (gimp-context-set-background '(0 0 0))
  (gimp-selection-all image)
  (gimp-image-add-layer image frame 0)
  (if (= usebackgnd FALSE)
    (gimp-edit-fill frame BACKGROUND-FILL)
    (gimp-edit-clear frame)
    )
  (gimp-context-set-background '(255 255 255))
  (gimp-selection-layer-alpha drawable)
  (gimp-edit-fill frame BACKGROUND-FILL)
  (gimp-drawable-set-visible frame FALSE)
  (gimp-drawable-set-visible drawable FALSE)
  (set! count 0)
  (while (< count pframes)
    (script-fu-fireanim-doit image frame drawable xoffs yoffs usebackgnd)
    (set! count (+ count 1))
    )
  (set! larray (cons-array (+ nframes oframes 1) 'byte))
  (set! count 1)
  (while (<= count (+ nframes oframes))
    (script-fu-fireanim-doit image frame drawable xoffs yoffs usebackgnd)
    (set! newlayer (car (gimp-layer-copy frame TRUE)))
    (gimp-image-add-layer image newlayer 0)
    (script-fu-fireanim-prep image newlayer drawable gradient)
    (if (= usebackgnd TRUE)
      (begin
        (set! bglayer (car (gimp-layer-copy bgcopy TRUE)))
        (gimp-image-add-layer image bglayer 1)
        (gimp-drawable-set-visible bglayer TRUE)
        (gimp-drawable-set-visible newlayer TRUE)
        (set! newlayer (car (gimp-image-merge-visible-layers image 2)))
        )
      )
    (aset larray count newlayer)
    (gimp-drawable-set-name newlayer (string-append "Frame "
                                                    (number->string count) " ("
                                                    (number->string framerate) "ms) (replace)"
                                                    ))
    (gimp-drawable-set-visible newlayer FALSE)
    (set! count (+ count 1))
    )
  (gimp-drawable-set-visible drawable FALSE)
  (set! count 1)
  (while (<= count oframes)
    (set! opac (* 100 (- 1.0 (/ count (+ oframes 1)))))
    (set! framea (aref larray count))
    (set! frameb (aref larray (+ count nframes)))
    (gimp-drawable-set-visible framea TRUE)
    (gimp-drawable-set-visible frameb TRUE)
    (gimp-layer-set-opacity frameb opac)
    (set! framea (car (gimp-image-merge-visible-layers image 2)))
    (aset larray count framea)
    (gimp-drawable-set-visible framea FALSE)
    (set! count (+ count 1))
    )
  (set! count 1)
  (while (<= count nframes)
    (set! framea (aref larray count))
    (gimp-drawable-set-visible framea TRUE)
    (set! count (+ count 1))
    )
  (gimp-image-set-active-layer image framea)
  (gimp-image-remove-layer image drawable)
  (gimp-image-remove-layer image frame)
  (if (= usebackgnd TRUE)
    (gimp-image-remove-layer image bgcopy)
    )
  (gimp-display-new image)
  (gimp-selection-none image)
  (gimp-selection-none origimage)
  (gimp-image-undo-enable image)
  (gimp-displays-flush)
 )
)
(script-fu-register
    "script-fu-fireanim"
    "<Image>/FX-Foundry/Animation/Fire Animator"
    "FireAnim"
    "Vidar Madsen <vidar@prosalg.no>"
    "Vidar Madsen"
    "16. March 2001"
    "RGBA"
    SF-IMAGE "Input Image" 0                    ;gds
    SF-DRAWABLE "Input Drawable" 0                ;gds
    SF-ADJUSTMENT "# of frames" '(15 2 255 1 1 0 1)            ;gds
    SF-ADJUSTMENT "Overlap frames" '(6 0 255 1 1 0 1)        ;gds
    SF-ADJUSTMENT "Prep frames" '(15 0 255 1 1 0 1)            ;gds
    SF-GRADIENT "Gradient" "Incandescent"                ;gds
    SF-ADJUSTMENT "Framerate (ms)" '(50 1 1000 10 10 0 1)        ;gds
    SF-ADJUSTMENT "Angle (Degrees)" '(0 0 360 .1 .1 1 1)        ;gds
    SF-ADJUSTMENT "Distance" '(2 0 1000 1 1 0 1)            ;gds
    SF-TOGGLE "Use background?" FALSE
    SF-DRAWABLE "Background" 0
)

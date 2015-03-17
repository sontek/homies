;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Spread out layers script  for GIMP 2.4
; Created by Akkana Peck
;
; Tags: layers, transpose, move, spread, panorama
;
; Author statement:
;
;; Start by opening all the images of your panorama as separate
;; layers in a single image, with the bottom layer being the leftmost.
;; Then run pandora-combine on that image.
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



(define (script-fu-pandora-combine img drawable
                                   overlap top-on-right use-mask)
  ;; Calculate the size for the new image
  (let* ((layers (gimp-image-get-layers img))
         (num-layers (car layers))
         (layer-array (cadr layers))
         (bottomlayer (aref layer-array (- num-layers 1)))
         ; Pandora assumes that all layers are the same size as the first:
         ; XXX change this eventually.
         (layer-w (car (gimp-drawable-width bottomlayer)))
         (layer-h (car (gimp-drawable-height bottomlayer)))
         (overlap-frac (/ overlap 100))
         (extra-frac (- 1.0 overlap-frac))
         (hslop (/ layer-w 4))
         (vslop (/ (* hslop 3) 2))
         (pan-img-w (* layer-w (+ 1 (* (- num-layers 0.3) (- 1 overlap-frac)))))
         (pan-img-h (+ layer-h vslop))
         (newy (/ vslop 2))
         (i (- num-layers 1) 1)    ; start from the bottom layer
         )
    ;(gimp-message (number->string pan-img-w))
    (gimp-image-undo-group-start img)
    (gimp-image-resize img pan-img-w pan-img-h 0 0)

    ;; Loop over the layers starting with the second, moving each one.
    ;; Layers are numbered starting with 0 as the top layer in the stack.
    ;(gimp-layer-translate bottomlayer 0 newy)
    (gimp-context-push)
    (while (>= i 0)
           ;(gimp-message (number->string i))
           (let* ((thislayer (aref layer-array i))
                  (thislayer-w (car (gimp-drawable-width thislayer)))
                  (newx (if (= top-on-right TRUE)
                            (* (- (- num-layers i) 1)
                               (* thislayer-w extra-frac))
                            (* i (* thislayer-w extra-frac))
                            ))
                  )
             (if (= (car (gimp-layer-is-floating-sel thislayer)) FALSE)
                 (begin
                  (gimp-layer-translate thislayer newx newy)
                  (if (and (= use-mask TRUE)
                           (= (car (gimp-layer-get-mask thislayer)) -1)
                           (not (= i (- num-layers 1))))
                      (let* ((masklayer (car (gimp-layer-create-mask
                                              thislayer ADD-BLACK-MASK)))
                             (grad-w (* (* layer-w overlap-frac) 0.5))
                             (grad-start (if (= top-on-right TRUE)
                                             grad-w
                                             (- thislayer-w grad-w)))
                             (grad-end (if (= top-on-right TRUE)
                                           0 thislayer-w))
                             )
                        (gimp-layer-add-alpha thislayer)
                        (gimp-layer-add-mask thislayer masklayer)
                        (gimp-context-set-foreground '(255 255 255))
                        (gimp-context-set-background '(0 0 0))
                        (gimp-edit-blend masklayer FG-BG-RGB-MODE NORMAL-MODE
                                         GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE
                                         FALSE 0 0 TRUE
                                         grad-start 0 grad-end 0)
                        (gimp-layer-set-edit-mask thislayer FALSE)
                        ))))
             )
           (set! i (- i 1))
           )
    (gimp-context-pop)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    )
)

(script-fu-register "script-fu-pandora-combine"
                         _"<Image>/FX-Foundry/Multi-Layer Tools/Spread out layers ..."
                         _"Line up layers as a panorama"
                         "Akkana Peck"
                         "Akkana Peck"
                         "June 2006"
                         "*"
                         SF-IMAGE       "Image"              0
                         SF-DRAWABLE    "Drawable"           0
                         SF-ADJUSTMENT _"Overlap (percent)"  '(50 0 100 1 10 0 1)
                         SF-TOGGLE     _"Top Layer on Right" TRUE
                         SF-TOGGLE     _"Use Layer Masks"    TRUE
)



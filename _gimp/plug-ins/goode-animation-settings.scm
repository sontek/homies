;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Animation settings script  for GIMP 2.4
; Created by Saul Goode
;
; Tags: animation, tool
;
; Author statement:
;
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


(define (script-fu-anim-settings image drawable req-delay req-mode)
  (define (visible-layers img)
    ;; This routine returns a list of visible layers in an image
    (let* (
        (all-layers (gimp-image-get-layers img))
        (i (car all-layers))
        (viewable ())
        (tmp FALSE)
        )
      (set! all-layers (cadr all-layers))
      (while (> i 0)
        (set! tmp (car (gimp-drawable-get-visible (aref all-layers (- i 1)))))
        (if (= tmp TRUE)
          (set! viewable (append viewable (cons (aref all-layers (- i 1)) 'byte))) ;; append
          )
        (set! i (- i 1))
        )
      viewable
      )
    )
  (gimp-image-undo-group-start image)
  (let* (
      (layers 0)
      (full-name 0)
      (par-name 0)
      (layer-name 0)
      (mode 0)
      (delay "")
      )
    (set! layers (visible-layers image))
    (while (pair? (car layers))
      (cond
        ( (or (= req-mode 0) (= req-mode 3))
          (set! mode "")
          )
        ( (= req-mode 1)
          (set! mode "(combine)")
          )
        ( (= req-mode 2)
          (set! mode "(replace)")
          )
        )
      (set! full-name (strbreakup (car (gimp-drawable-get-name (car layers))) "("))
      (set! layer-name (car full-name))
      (set! full-name (cdr full-name))
      (while (pair? full-name) ;; for each parenthetical, see if it is a delay or a mode (it might be a delay)
        (set! par-name (string-trim (car full-name)))
        (cond
          ( (= 0 (strcmp "combine)" (string-downcase par-name)))
            (if (= req-mode 0) ;; Keep
              (set! mode "(combine)")
              )
            )
          ( (= 0 (strcmp "replace)" (string-downcase par-name)))
            (if (= req-mode 0) ;; Keep
              (set! mode "(replace)")
              )
            )
          ( (< (strcspn "0123456789" (substring par-name 0 1)) 10)
            (if (= req-delay 0)
              (set! delay (string-append "(" par-name))
              (set! delay "") ;; clear delay for -1 or for positive request
              )
            )
          )
        (set! full-name (cdr full-name))
        )
      (if (> req-delay 0)
        (set! delay (string-append "(" (number->string req-delay 10) "ms) "))
        )
      (gimp-drawable-set-name (car layers) (string-append layer-name delay mode))
      (set! layers (cdr layers))
      )
    )
  (gimp-image-undo-group-end image)
  (gimp-displays-flush)
  )

(script-fu-register "script-fu-anim-settings"
 "<Image>/FX-Foundry/Animation/_Change Settings..." ;; might like to keep it separate from the standard plugins
 "Sets the Frame delay and mode of GIF animations by renaming visible layers."
 "Saul Goode"
 "Saul Goode"
 "3/11/2006"
 ""
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 SF-ADJUSTMENT "Delay: (1-10000 mS, 0=Keep, -1=Clear)" '( 100 -1 10000 1 10 0 1 )
 SF-OPTION "Mode:" '( "Keep" "(combine)" "(replace)" "Clear")
 )

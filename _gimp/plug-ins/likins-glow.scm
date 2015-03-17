;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Selection glow script  for GIMP 2.4
; Copyright (c) 1997 Adrian Likins
; aklikins@eos.ncsu.ed
;
; Tags: glow, selection, effect
;
; Author statement:
;  Makes a "glow" around the outside of the current selection
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;
; Changed on June 15, 2000 by Kevin Cozens <kcozens@interlog.com>
; Updated for GIMP 1.1.26
;
; Changed on January 29, 2004 by Kevin Cozens <kcozens@interlog.com>
; Updated for GIMP 2.0pre3
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

(define (my-pair->string pair)
  (cons ((pair? (cdr pair))
     (my-list->string pair))
    (else
     (print "aoeuo")
     (string-append "("   (to-string (car pair))
            " . " (to-string (cdr pair)) ")"))))

(define (flatten-string-list str lst)
  (cond ((not (null? lst))
     (flatten-string-list (string-append str (car lst) " ")
                  (cdr lst)))
    (else
     str)))

(define (my-list->string pair)
  (let ((string-list (flatten-string-list ""
              (map (lambda (x) (to-string x)) pair))))
    (string-append "(" string-list ")")))

(define (to-string arg)
  (cond ((number? arg)
     (number->string arg))
    ((string? arg)
     arg)
    ((symbol? arg)
     " <symbol is unhandeld> ")
    ((pair? arg)
     (my-pair->string arg))
    (else
     " <unhandeled> ")))

(define (message-box . args)
  (gimp-message (apply string-append (map to-string args))))


(define (script-fu-glow image
            drawable
            glow-radius
            feather-radius
            glow-color
            glow-opacity
            keep-selection)


    (define seperate-layer TRUE)

    (if (= (car (gimp-selection-is-empty image)) TRUE)
    (begin
      (message-box "The current image doesn't have a selection.\n\nThis plugin creates a glow effect around a \nSELECTED AREA of the image."))
    (begin
      (define from-selection TRUE)
      (define active-selection (car (gimp-selection-save image)))
;Start an undo group so the process can be undone with one undo
  (gimp-image-undo-group-start image)     ;Lets wait with the undo group untill we know if there is a selection.
  (let* (
     (type (car (gimp-drawable-type-with-alpha drawable)))
     (old-gradient (car (gimp-gradients-get-gradient)))
     (old-fg (car (gimp-palette-get-foreground)))
     (old-bg (car (gimp-palette-get-background))))

    (gimp-layer-add-alpha drawable)

    (define selection-bounds (gimp-selection-bounds image))
    (define select-offset-x (cadr selection-bounds))
    (define select-offset-y (caddr selection-bounds))
    (define select-width (- (cadr (cddr selection-bounds)) select-offset-x))
    (define select-height (- (caddr (cddr selection-bounds)) select-offset-y))
    (define buffer (+ (* glow-radius 2) (* feather-radius 2) 2))
    (define select-height (+ select-height buffer))
    (define select-width (+ select-width buffer))
    (define select-offset-x (- select-offset-x (/ buffer 2)))
    (define select-offset-y (- select-offset-y (/ buffer 2)))

    (if (= seperate-layer TRUE)
    (begin
      (define effect-layer (car (gimp-layer-new image
                          select-width
                          select-height
                          type
                          "glow layer"
                          100
                          NORMAL-MODE)))
      (gimp-layer-set-offsets effect-layer select-offset-x select-offset-y)
      (gimp-image-add-layer image effect-layer -1)
      (gimp-selection-none image)
      (gimp-edit-clear effect-layer)
      (gimp-selection-load active-selection)
      (gimp-image-set-active-layer image effect-layer ))
    (begin
      (gimp-edit-copy drawable)))
    (define active-layer (car (gimp-image-get-active-layer image)))

    (gimp-selection-grow image glow-radius)
    (gimp-selection-feather image feather-radius)
    (gimp-palette-set-background glow-color)
    (gimp-edit-fill active-layer BACKGROUND-FILL)

    (if (= seperate-layer TRUE)
    (begin
      (gimp-selection-load active-selection)
      (gimp-edit-clear active-layer)
      (gimp-layer-set-opacity active-layer glow-opacity))
    (begin
      (gimp-selection-load active-selection)
      (let ((floating-sel (car (gimp-edit-paste active-layer FALSE))))
          (gimp-floating-sel-anchor floating-sel))
      (gimp-selection-load active-selection)))

    (gimp-gradients-set-gradient old-gradient)
    (gimp-palette-set-background old-bg)
    (gimp-palette-set-foreground old-fg)

    (if (= keep-selection FALSE)
    (gimp-selection-none image))

    (gimp-image-set-active-layer image drawable)
    (gimp-image-remove-channel image active-selection)

;Finish the undo group for the process
    (gimp-image-undo-group-end image)

    (gimp-displays-flush)))
    ))

(script-fu-register "script-fu-glow"
            "<Image>/FX-Foundry/Selection Effects/Glow Selection"
            "Makes a glow around the outside of the current selection."
            "Adrian Likins <adrian@gimp.org>"
            "Adrian Likins"
            "10/12/97"
            "RGB RGBA GRAY GRAYA"
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Drawable" 0
            SF-VALUE "Glow Radius" "2"
            SF-VALUE "Feather Radius" "10"
            SF-COLOR "Glow Color" '(255 255 255)
            SF-VALUE "Glow Opacity (only for seperate layer)" "100"
            SF-TOGGLE "Keep Selection?" TRUE)

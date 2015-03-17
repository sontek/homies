
;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Mats and Frames script  for GIMP 2.4
; Copyright (C) 2003 Eric Jeschke (eric@redskiesatnight.com)
; Based on code by
; Copyright (C) 1997 Andy Thomas (alt@picnic.demon.co.uk)
; and
; Copyright (C) 1997 Andrew Donkin  (ard@cs.waikato.ac.nz)
;
; Tags: border
;
; Author statement:
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Version 0.2
; Fixed bug working with GIMP 2.2; need to add drawable before editing
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


; gimp-scheme should add this...it's a scheme "standard"
(define (cadddr l) (caddr (cdr l)))

; ditto
; non-functional mapper (discards values, used for effect).
(define (for-each proc l)
  (if (not (null? l))
      (begin
        (proc (car l))
        (for-each proc (cdr l)))))

; Create an array from a list.
(define (list-to-array contents)
  (let* ((alen (length contents))
         (n_array (cons-array alen 'double))
         (count 0))
    (for-each (lambda (val)
                (aset n_array count val)
                (set! count (+ count 1)))
              contents)
    n_array))

; Takes a color (represented as a list of numbers: (R G B)) and a delta
; value and returns four new colors representing the left, top, right and
; bottom shades.  Used for coloring the mat bevel.
;
; returns (LC TC RC BC)
(define (getcolors color delta)
  (letrec ((deltacolor (lambda (v d) (max (min (+ v d) 255) 0)))
           (adjcolor (lambda (cl d)
                       (mapcar (lambda (v) (deltacolor v d)) cl)))
           )
    (list
     (adjcolor color (- 0 (/ delta 2)))   ; left color
     (adjcolor color (- 0 delta))         ; top color
     (adjcolor color (/ delta 2))         ; right color
     (adjcolor color delta)               ; bottom color
     )
    ))

; utility routine to decipher color choice option and return the
; appropriate fill: from dialog color picker, current fg, or current bg.
(define (get-color fillchoice colorchoice)
    (cond
     ((= fillchoice 0) colorchoice)
     ((= fillchoice 1) (car (gimp-palette-get-foreground)))
     (TRUE (car (gimp-palette-get-background))) ))

; Draw a border around an image.
;
; This is the workhorse function.  Draws a constant width border around
; an image with optional padding on each of the 4 sides.  If 'delta' > 0
; then each side will be colored so that it resembles a bevel (unless
; the fillchoice is a pattern).
(define (draw-border drawable
                     borderwidth
                     lpad tpad rpad bpad   ; # pixels to pad on L, T, R, B
                     fillchoice            ; type of fill
                     fillcolor delta       ; fill parameters
                     bumppattern           ; "texture" pattern
                     leavebumpmap          ; flag: TRUE-->preserve bumpmap
                     ibumpp                ; flag: TRUE-->bump interactively
                     layername             ; border is drawn on a new layer
                     leaveselectionp       ; if true, border becomes selection
                     )
  (let* ((img (car (gimp-drawable-image drawable)))
         (imgtype (car (gimp-drawable-type-with-alpha drawable)))
         (owidth (car (gimp-image-width img)))      ; current image w & h
         (oheight (car (gimp-image-height img)))
         (old-fg (car (gimp-palette-get-foreground)))
         (old-bg (car (gimp-palette-get-background)))
         ; calculate new dimensions
         (width (+ owidth (* 2 borderwidth) lpad rpad))
         (height (+ oheight (* 2 borderwidth) tpad bpad))
         ; calculate coordinates for filling
         (ulimage_x (+ borderwidth lpad))
         (ulimage_y (+ borderwidth tpad))
         (llimage_x ulimage_x)
         (llimage_y (+ ulimage_y oheight))
         (lrimage_x (+ llimage_x owidth))
         (lrimage_y llimage_y)
         (urimage_x lrimage_x)
         (urimage_y ulimage_y)
         (ulborder_x lpad)
         (ulborder_y tpad)
         (llborder_x ulborder_x)
         (llborder_y (+ llimage_y borderwidth))
         (lrborder_x (+ lrimage_x borderwidth))
         (lrborder_y (+ lrimage_y borderwidth))
         (urborder_x lrborder_x)
         (urborder_y ulborder_y)
         ; get colors for each side (returns LC TC RC BC)
         (filltype BG-BUCKET-FILL)
         (color (get-color fillchoice fillcolor))
         (colors (getcolors color delta))
         ; create a new layer above current one
         (layer (car (gimp-layer-new img width height imgtype layername
                                     100 NORMAL)))
         (ibumpflag (if (= ibumpp TRUE) 0 1))
         )

    ; fill with transparent pixels and resize to new dimensions
    (gimp-drawable-fill layer TRANS-IMAGE-FILL)
    (gimp-image-resize img width height ulimage_x ulimage_y)

    ; add the layer
    (gimp-image-add-layer img layer 0)

    ; set up for pattern fill if that was requested
    (if (> fillchoice 2)
        (set! filltype PATTERN-BUCKET-FILL))

    ; color the left polygon
    (gimp-palette-set-background (car colors))
    (gimp-free-select img
              10
              (list-to-array (list ulborder_x ulborder_y ulimage_x ulimage_y
                               llimage_x llimage_y llborder_x llborder_y
                               ulborder_x ulborder_y))
              REPLACE
              0
              0
              0.0)
    (gimp-bucket-fill layer filltype NORMAL-MODE 100 0 FALSE 0 0)

    ; color the left pad
    (if (> lpad 0)
        (begin
          (gimp-palette-set-background color)
          (gimp-free-select img
              10
              (list-to-array (list 0 ulborder_y ulborder_x ulborder_y llborder_x
                               llborder_y 0 llborder_y 0 ulborder_y))
              REPLACE
              0
              0
              0.0)
          (gimp-bucket-fill layer filltype NORMAL-MODE 100 0 FALSE 0 0)
          ))

    ; color the top polygon
    (gimp-palette-set-background (cadr colors))
    (gimp-free-select img
              10
              (list-to-array (list ulborder_x ulborder_y ulimage_x ulimage_y
                               urimage_x urimage_y urborder_x urborder_y
                               ulborder_x ulborder_y))
              REPLACE
              0
              0
              0.0)
    (gimp-bucket-fill layer filltype NORMAL-MODE 100 0 FALSE 0 0)

    ; color the top pad
    (if (> tpad 0)
        (begin
          (gimp-palette-set-background color)
          (gimp-free-select img
              10
              (list-to-array (list 0 0 0 ulborder_y width urborder_y width 0 0 0))
              REPLACE
              0
              0
              0.0)
          (gimp-bucket-fill layer filltype NORMAL-MODE 100 0 FALSE 0 0)
          ))

    ; color the right polygon
    (gimp-palette-set-background (caddr colors))
    (gimp-free-select img
              10
              (list-to-array (list urborder_x urborder_y urimage_x urimage_y
                               lrimage_x lrimage_y lrborder_x lrborder_y
                               urborder_x urborder_y))
              REPLACE
              0
              0
              0.0)
    (gimp-bucket-fill layer filltype NORMAL-MODE 100 0 FALSE 0 0)

    ; color the right pad
    (if (> rpad 0)
        (begin
          (gimp-palette-set-background color)
          (gimp-free-select img
              10
              (list-to-array (list width urborder_y urborder_x urborder_y
                               lrborder_x lrborder_y width lrborder_y
                               width urborder_y))
              REPLACE
              0
              0
              0.0)
          (gimp-bucket-fill layer filltype NORMAL-MODE 100 0 FALSE 0 0)
          ))

    ; color the bottom polygon
    (gimp-palette-set-background (cadddr colors))
    (gimp-free-select img
              10
              (list-to-array (list llborder_x llborder_y llimage_x llimage_y
                               lrimage_x lrimage_y lrborder_x lrborder_y
                               llborder_x llborder_y))
              REPLACE
              0
              0
              0.0)
    (gimp-bucket-fill layer filltype NORMAL-MODE 100 0 FALSE 0 0)

    ; color the bottom pad
    (if (> bpad 0)
        (begin
          (gimp-palette-set-background color)
          (gimp-free-select img
              10
              (list-to-array (list 0 height 0 llborder_y width lrborder_y
                               width height 0 height))
              REPLACE
              0
              0
              0.0)
          (gimp-bucket-fill layer filltype NORMAL-MODE 100 0 FALSE 0 0)
          ))

    ; if user wanted to texture the border, do a bump map
    (if (not (null? bumppattern))
        (let* ((old-pattern (car (gimp-patterns-get-pattern)))
               (bumpmap (car (gimp-layer-new img width height imgtype
                                             "Texture Bump Map" 100 NORMAL)))
               )
          (gimp-image-add-layer img bumpmap 0)
          (gimp-patterns-set-pattern bumppattern)
          ; NOTE: if we try to do a bump map on only the selection we get
          ; wierd artifacts around the edges; as a kludge, we turn on
          ; "preserve transparency" and bump map the entire thing.  It might
          ; also be possible to just grow the selection, but I haven't tried
          ; that yet.
          (gimp-layer-set-preserve-trans layer 1)
          (gimp-selection-all img)
          (gimp-bucket-fill bumpmap PATTERN-BUCKET-FILL NORMAL-MODE 100
                            0 FALSE 0 0)
          (gimp-patterns-set-pattern old-pattern)
          (plug-in-bump-map ibumpflag img layer bumpmap 125 45 3 0 0 0 0
                            TRUE FALSE 1)
          (if (= leavebumpmap TRUE)
              (gimp-layer-set-visible bumpmap 0)
              (gimp-image-remove-layer img bumpmap)
              )
          (gimp-layer-set-preserve-trans layer 0)
      ))

    ; clear selection
    (gimp-selection-none img)

    ; set selection to border if desired
    (if (= leaveselectionp TRUE)
        (begin
          (gimp-free-select img
              10
              (list-to-array (list ulimage_x ulimage_y urimage_x urimage_y
                               lrimage_x lrimage_y llimage_x llimage_y
                               ulimage_x ulimage_y))
              REPLACE
              0
              0
              0.0)
          (gimp-selection-invert img)
          ))

    ; clean-up
    (gimp-palette-set-background old-bg)

    ; return value
    (list layer)
  );end let*
)

; Script to add a mat around an image.
;
(define (script-fu-add-mat image drawable
         bevelwidth           ; mat bevel width in pixels
         bevelfillchoice      ; {0 (Choose) | 1 (FG color) | 2 (BG color)}
         bevelcolorchoice     ; (Choose) color from color selection dialog
         delta                ; amount to vary fill to create bevel effect
         matwidth
         matfillchoice        ; as above, but for the mat
         matcolorchoice
         texturep             ; flag: TRUE-->bump map the mat
         mattexture           ; pattern for bump mapping mat
         leavebumpmapp        ; flag: TRUE-->preserve bump map
         ibumpp               ; flag: TRUE-->bump interactively
         lpad                 ; amount to pad the mat on the left
         tpad                 ; top
         rpad                 ; right
         bpad                 ; bottom
         layersp              ; flag: TRUE-->preserve new layers
         leaveselectionp      ; flag: TRUE-->mat is selected on exit
         )
  (let* ((img (car (gimp-drawable-image drawable)))
         (pattern (if (= texturep TRUE) mattexture '()))
         )

    ; checkpoint for undo
    (gimp-undo-push-group-start img)

    ; draw bevel
    (if (> bevelwidth 0)
        (draw-border drawable bevelwidth 0 0 0 0 bevelfillchoice
                    bevelcolorchoice delta '() FALSE FALSE "Mat Bevel" FALSE)
        )

    ; draw mat
    (if (> matwidth 0)
        (draw-border drawable matwidth lpad tpad rpad bpad
                    matfillchoice matcolorchoice 0 pattern leavebumpmapp
                    ibumpp "Mat" leaveselectionp)
        )

    ; merge layers if user did not want them
    (if (= layersp FALSE)
        (begin
          (if (> matwidth 0)
              (let ((layer (car (gimp-image-get-active-layer img))))
                (gimp-image-merge-down img layer EXPAND-AS-NECESSARY) ))
          (if (> bevelwidth 0)
              (let ((layer (car (gimp-image-get-active-layer img))))
                (gimp-image-merge-down img layer EXPAND-AS-NECESSARY) ))
          ))

    ; th-th-th-that's all folks!
    (gimp-undo-push-group-end img)
    (gimp-displays-flush)
    )
)


; Script to add a frame around an image.
;
(define (script-fu-add-frame image drawable
         framewidth
         framefillchoice      ; as above, but for the mat
         framecolorchoice
         texturep             ; flag: TRUE-->bump map the mat
         frametexture         ; pattern for bump mapping mat
         leavebumpmapp        ; flag: TRUE-->preserve texture bump map
         ibumpp               ; flag: TRUE-->bump interactively
         bevelindex           ; "depth" of 3D effect
         ds-width             ; inner shadow width
         ds-opacity           ; inner shadow opacity
         layersp              ; flag: TRUE-->preserve new layers
         leaveselectionp      ; flag: TRUE-->mat is selected on exit
         leavebevelbumpmapp   ; flag: TRUE-->preserve bevel bump map
         )
  (let* ((img (car (gimp-drawable-image drawable)))
         (imgtype (car (gimp-drawable-type-with-alpha drawable)))
         (pattern (if (= texturep TRUE) frametexture '()))
         )

    ; checkpoint for undo
    (gimp-undo-push-group-start img)

    ; parameter sanity checks
    (if (<= framewidth 0)
        (set! framewidth 10))
    (if (> bevelindex (/ framewidth 2))
        (set! bevelindex (/ framewidth 2)))

    ; draw border
    (let* ((frame (car (draw-border drawable framewidth 0 0 0 0
                                    framefillchoice framecolorchoice 0
                                    pattern leavebumpmapp ibumpp "Frame"
                                    TRUE)))
           (selection (car (gimp-selection-save img)))
           (width (car (gimp-image-width img)))      ; current image w & h
           (height (car (gimp-image-height img)))
           (old-bg (car (gimp-palette-get-background)))
           (bumpmap (car (gimp-layer-new img width height imgtype
                                         "Frame Bevel Bump Map" 100 NORMAL)))
           (index 1)
           )

      ; Initialise our bumpmap
      (gimp-image-add-layer img bumpmap 0)
      (gimp-layer-set-visible bumpmap 0)
      (gimp-palette-set-background '(0 0 0))
      (gimp-drawable-fill bumpmap BG-IMAGE-FILL)
      (gimp-selection-load selection)

      ; Fill with a gradient of sorts
      ; TODO: there has got to be a more efficient way to do this
      ; (gradient fills?)
      (while (< index bevelindex)
             (let ((gv (/ (* index 255) bevelindex)))
               (begin
                 (gimp-palette-set-background (list gv gv gv))
                 (gimp-bucket-fill bumpmap BG-BUCKET-FILL NORMAL 100 0
                                   FALSE 0 0)
                 (gimp-selection-shrink img 1)
                 (set! index (+ index 1))
                 )))

      ; Now the white interior of the bumpmap
      (gimp-palette-set-background '(255 255 255))
      (gimp-bucket-fill bumpmap BG-BUCKET-FILL NORMAL 100 0 FALSE 0 0)

      ; Do the bump map.
      (gimp-selection-none img)
      (plug-in-bump-map 1 img frame bumpmap 125 45 3 0 0 0 0 TRUE FALSE 1)

      ; remove bump map if user did not want it
      (if (= leavebevelbumpmapp FALSE)
          (gimp-image-remove-layer img bumpmap)
          )

      ; Now for the inner shadow
      (if (> ds-width 0)
          (let* ((ds-color '(0 0 0))
                 ; create the shadow layer
                 (shadow (car (gimp-layer-new img width height imgtype
                                              "Shadow" ds-opacity NORMAL)))
                 )
            (gimp-image-add-layer img shadow 0)
            ;(gimp-image-set-active-layer img shadow)
            (gimp-selection-none img)
            (gimp-edit-clear shadow)
            (gimp-palette-set-background ds-color)
            (gimp-selection-load selection)
            (gimp-selection-feather img ds-width)
            (gimp-edit-fill shadow BG-IMAGE-FILL)
            (gimp-selection-load selection)
            (gimp-edit-clear shadow)

;             (if (= layersp FALSE)
;                 (begin
;                   ;(gimp-image-set-active-layer img shadow)
;                   (gimp-image-merge-down img shadow EXPAND-AS-NECESSARY)
;                 ))
            ))

      ; merge layers if user did not want them
      (if (= layersp FALSE)
          (begin
            ;(gimp-image-set-active-layer img frame)
            ;(gimp-image-merge-down img frame EXPAND-AS-NECESSARY)
            (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY)
            ))

      ; leave the selection if the user requested it
      (if (= leaveselectionp TRUE)
          (gimp-selection-load selection)
          (gimp-selection-none img)
          )

      ; clean up
      (gimp-image-remove-channel img selection)
      (gimp-palette-set-background old-bg)

     );end let*

    ; th-th-th-that's all folks!
    (gimp-undo-push-group-end img)
    (gimp-displays-flush)
    )
)

; Obligatory script registrations...

(script-fu-register "script-fu-add-mat"
            "<Image>/FX-Foundry/Image Effects/Add Mat..."
            "Add a single mat around an image"
            "Eric Jeschke <eric@redskiesatnight.com>"
            "Eric Jeschke"
            "5/27/03"
            "RGB* GRAY*"
            SF-IMAGE "Input Image" 0
            SF-DRAWABLE "Input Drawable" 0
            SF-ADJUSTMENT _"Bevel Width" '(5 0 250 1 10 0 1)
            SF-OPTION _"Bevel Fill" '(_"Color" _"FG color" _"BG color" _"Pattern")
            SF-COLOR _"Bevel Fill Color" '(221 221 221)
            SF-ADJUSTMENT _"Delta Value on Bevel Color" '(25 1 255 1 10 0 1)
            SF-ADJUSTMENT _"Mat Width" '(35 0 1000 1 10 0 1)
            SF-OPTION _"Mat Fill" '(_"Color" _"FG color" _"BG color" _"Pattern")
            SF-COLOR _"Mat Fill Color" '(128 128 128)
            SF-TOGGLE  _"Texture Mat" FALSE
            SF-PATTERN _"Texture Pattern" _"Wood"
            SF-TOGGLE  _"Leave Texture Bump Map" FALSE
            SF-TOGGLE  _"Bump Interactively" FALSE
            SF-ADJUSTMENT _"Left Pad" '(0 0 1000 1 10 0 1)
            SF-ADJUSTMENT _"Top Pad" '(0 0 1000 1 10 0 1)
            SF-ADJUSTMENT _"Right Pad" '(0 0 1000 1 10 0 1)
            SF-ADJUSTMENT _"Bottom Pad" '(0 0 1000 1 10 0 1)
            SF-TOGGLE  _"Use Layers" FALSE
            SF-TOGGLE  _"Leave Selection" FALSE
            )

(script-fu-register "script-fu-add-frame"
            "<Image>/FX-Foundry/Image Effects/Add Frame..."
            "Add a frame around an image"
            "Eric Jeschke <eric@redskiesatnight.com>"
            "Eric Jeschke"
            "5/27/03"
            "RGB* GRAY*"
            SF-IMAGE "Input Image" 0
            SF-DRAWABLE "Input Drawable" 0
            SF-ADJUSTMENT _"Frame Width" '(35 0 1000 1 10 0 1)
            SF-OPTION _"Frame Fill" '(_"Color" _"FG color" _"BG color" _"Pattern")
            SF-COLOR _"Frame Fill Color" '(128 128 128)
            SF-TOGGLE  _"Texture Frame" FALSE
            SF-PATTERN _"Texture Pattern" _"Wood"
            SF-TOGGLE  _"Leave Texture Bump Map" FALSE
            SF-TOGGLE  _"Bump Interactively" FALSE
            SF-ADJUSTMENT _"Beveling Index" '(10 0 250 1 10 0 1)
            SF-ADJUSTMENT _"Inner Shadow Width" '(8 0 100 1 10 0 1)
            SF-ADJUSTMENT _"Inner Shadow Opacity" '(50 0 100 1 10 0 1)
            SF-TOGGLE  _"Use Layers" FALSE
            SF-TOGGLE  _"Leave Selection" FALSE
            SF-TOGGLE  _"Leave Bevel Bump Map" FALSE
            )

;
; END mats-and-frames.scm

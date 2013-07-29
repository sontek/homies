;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Conte draw tools script  for GIMP 2.4
; Author  : Arch. Giuseppe Conte
; Date    : 24 settembre 2003
; Revision: 22 maggio 2004
; Version : 2.0
; Last version at: http://xoomer.virgilio.it/lwcon/
; Help guide at  : http://xoomer.virgilio.it/lwcon/
; Arch. Giuseppe Conte <http://space.tin.it/edicola/lwcon/>
; Studio Tecnico Arch. Giuseppe Conte
; 72026 - San Pancrazio Salentino (BR) - Italy
;
; Tags: shapes
;
; Author statement:
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Updated to Gimp2.4 (11-2007) http://www.gimpscripts.com
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


(define (script-fu-draw-box inImage inLayer inXorig inYorig inWidth inHeight FeatherRadius Feather)

  (gimp-rect-select inImage
            inXorig inYorig
            inWidth inHeight
            REPLACE
            Feather        ; No feathering
            FeatherRadius)
  (gimp-edit-fill inLayer 0)
  (gimp-selection-none inImage)

  (gimp-displays-flush)
)

(script-fu-register
 "script-fu-draw-box"
 _"<Image>/FX-Foundry/Shapes/Parametric/Box"
 "Draw a box by filling in numeric values"
 "Arch. Giuseppe Conte"
 "2003, Giuseppe Conte"
 "13 Agosto 2003 - 72026 San Pancrazio Salentino (BR) - ITALY"
 "RGB* GRAY* INDEXED*"
 SF-IMAGE "The Image" 0
 SF-DRAWABLE "The Layer" 0
 SF-ADJUSTMENT "X origin" '(0 0 9999 1 10 0 1)
 SF-ADJUSTMENT "Y origin" '(0 0 9999 1 10 0 1)
 SF-ADJUSTMENT "Width" '(0 0 9999 1 10 0 1)
 SF-ADJUSTMENT "Height" '(0 0 9999 1 10 0 1)
 SF-ADJUSTMENT "Radius Feather" '(0 0 9999 1 10 0 1)
 SF-TOGGLE     "Feather" FALSE
)

;Draw arch

;
; Define the function:
(define (lung))
(define (angolo))
(define (arco))
(define (aRotaz))
(define (beta))
(define (gamma))
(define (gcount))
(define (npoint))
(define (script-fu-draw-arch inImage inLayer dx dy Radius From To Lati)

        (set! lung (- From To))
        (set! angolo (/ lung Lati))
        (set! arco (* angolo (/ 3.14 180)))
        (set! aRotaz (* To (/ 3.14 180)))
        (set! beta aRotaz)
        (set! gamma 0)
        (set! gcount 1)
        (set! npoint 4)
    (let* (    (segment (cons-array 4 'double))
        (stepx dx)
        (stepy dy)
        (raggio Radius)
        (nlati Lati)
          )

;inizio delle operazioni che potranno essere annullate con un solo undo
    (gimp-undo-push-group-start inImage)

  (while (<= gcount nlati)

          (set! beta (+ beta arco))
          (set! gamma (- beta arco))
          (aset segment 0 (+ (* raggio (cos gamma)) stepx))
          (aset segment 1 (+ (- (* raggio (sin gamma))) stepy))
          (aset segment 2 (+ (* raggio (cos beta)) stepx))
          (aset segment 3 (+ (- (* raggio (sin beta))) stepy))

        (gimp-pencil inLayer npoint segment )
          (set! gcount (+ gcount 1))

            (gimp-displays-flush)

   );end while

;fine delle operazioni che potranno essere annullate con un solo undo
    (gimp-undo-push-group-end inImage)

  );;let
) ;;def

(script-fu-register
 "script-fu-draw-arch"
 _"<Image>/FX-Foundry/Shapes/Parametric/_Arch"
 "Draw arch and circles, at center point specified, from initial angle to final angle."
 "Arch. Giuseppe Conte"
 "2004, Giuseppe Conte"
 "22 maggio 2004 - Ver. 2.0"
 "RGB* GRAY* INDEXED*"
 SF-IMAGE "The Image" 0
 SF-DRAWABLE "The Layer" 0
 SF-ADJUSTMENT "X center" '(0 -9999 9999 1 10 0 1)
 SF-ADJUSTMENT "Y center" '(0 -9999 9999 1 10 0 1)
 SF-ADJUSTMENT "Radius" '(10 0 9999 1 10 0 1)
 SF-ADJUSTMENT "From angle" '(0 -9999 9999 1 10 0 1)
 SF-ADJUSTMENT "To angle" '(360 -9999 9999 1 10 0 1)
 SF-ADJUSTMENT "Number of Segments" '(36 0 9999 1 10 0 1)
)

;;;
;;; draw-ellipse
;;;
;;; Updated to Gimp2.4 (11-2007) http://www.gimpscripts.com

;;; Define the function:
(define (angolo))
(define (arco))
(define (delta))
(define (alfa))
(define (xa))
(define (ya))
(define (xb))
(define (yb))
(define (gcount))
(define (npoint))

(define (script-fu-draw-ellipse inImage inLayer dx dy Ra Rb Lati Rotaz)

        (set! angolo (/ 360 Lati))
        (set! arco (* angolo (/ 3.14 180)))
        (set! delta (* Rotaz (/ 3.14 180)))

        (set! alfa 0)
        (set! xa 0)
        (set! ya 0)
        (set! xb 0)
        (set! yb 0)
        (set! gcount 1)
        (set! npoint 4)
    (let* (    (segment (cons-array 4 'double))
        (stepx dx)
        (stepy dy)

        (nlati Lati)
          )

;inizio delle operazioni che potranno essere annullate con un solo undo
    (gimp-undo-push-group-start inImage)

  (while (<= gcount nlati)

          (set! xa  (* Ra (cos alfa)) )
          (set! ya  (- (* Rb (sin alfa))) )

        (aset segment 0 (+ (- (* (cos delta) xa) (* (sin delta) ya)) stepx) )
        (aset segment 1 (+ (+ (* (sin delta) xa) (* (cos delta) ya)) stepy) )

         (set! alfa (+ alfa arco))

         (set! xb  (* Ra (cos alfa)) )
         (set! yb  (- (* Rb (sin alfa))) )

        (aset segment 2 (+ (- (* (cos delta) xb) (* (sin delta) yb)) stepx)   )
        (aset segment 3 (+ (+ (* (sin delta) xb) (* (cos delta) yb)) stepy) )

        (gimp-pencil inLayer npoint segment )
          (set! gcount (+ gcount 1))

   );end while

;fine delle operazioni che potranno essere annullate con un solo undo
    (gimp-undo-push-group-end inImage)

      (gimp-displays-flush)

  );;let
) ;;def

(script-fu-register
 "script-fu-draw-ellipse"
 _"<Image>/FX-Foundry/Shapes/Parametric/Ellipse"
 "Draw an ellipse."
 "Arch. Giuseppe Conte <http://space.tin.it/edicola/lwcon/>"
 "2002, Giuseppe Conte"
 "21 September 2002 - San Pancrazio Salentino (BR) - Italy"
 "RGB* GRAY* INDEXED*"
 SF-IMAGE "The Image" 0
 SF-DRAWABLE "The Layer" 0
 SF-ADJUSTMENT "X center" '(0 0 9999 1 10 0 1)
 SF-ADJUSTMENT "Y center" '(0 0 9999 1 10 0 1)
 SF-ADJUSTMENT "Semi-axis a" '(10 0 9999 1 10 0 1)
 SF-ADJUSTMENT "Semi-axis b" '(10 0 9999 1 10 0 1)
 SF-ADJUSTMENT "Number segments" '(36 0 9999 1 10 0 1)
 SF-ADJUSTMENT "Inclination" '(0 -9999 9999 1 10 0 1)
)
;;;
;;; draw-polygon
;;;
;;; Updated to Gimp2.4 (11-2007) http://www.gimpscripts.com

;;; Define the function:

(define (script-fu-draw-polygon inImage inLayer dx dy Radius Lati)

        (set! angolo (/ 360 Lati))
        (set! arco (* angolo (/ 3.14 180)))
        (set! beta 0)
        (set! gamma 0)
        (set! gcount 1)
        (set! npoint 4)
    (let* (    (segment (cons-array 4 'double))
        (stepx dx)
        (stepy dy)
        (raggio Radius)
        (nlati Lati)
          )

;inizio delle operazioni che potranno essere annullate con un solo undo
    (gimp-undo-push-group-start inImage)

  (begin
  (while (<= gcount nlati)

          (set! beta (+ beta arco))
          (set! gamma (- beta arco))
          (aset segment 0 (+ (* raggio (cos gamma)) stepx))
          (aset segment 1 (+ (- (* raggio (sin gamma))) stepy))
          (aset segment 2 (+ (* raggio (cos beta)) stepx))
          (aset segment 3 (+ (- (* raggio (sin beta))) stepy))

        (gimp-pencil inLayer npoint segment )
          (set! gcount (+ gcount 1))

   );end while
   );end begin

;fine delle operazioni che potranno essere annullate con un solo undo
    (gimp-undo-push-group-end inImage)

      (gimp-displays-flush)

  );;let
) ;;def

(script-fu-register
 "script-fu-draw-polygon"
 _"<Image>/FX-Foundry/Shapes/Parametric/Polygon"
 "Draw all regular polygonos by exact numbers. Input: center, radius, number segment."
 "Arch. Giuseppe Conte <http://space.tin.it/edicola/lwcon/>"
 "2002, Giuseppe Conte"
 "08 Maggio 2002 - Florence - Italy"
 "RGB* GRAY* INDEXED*"
 SF-IMAGE "The Image" 0
 SF-DRAWABLE "The Layer" 0
 SF-ADJUSTMENT "X center" '(0 0 9999 1 10 0 1)
 SF-ADJUSTMENT "Y center" '(0 0 9999 1 10 0 1)
 SF-ADJUSTMENT "Radius" '(10 0 9999 1 10 0 1)
 SF-ADJUSTMENT "Segments" '(3 0 9999 1 10 0 1)
)


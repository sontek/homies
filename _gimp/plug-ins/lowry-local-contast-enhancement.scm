;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Local Contrast Enhancement script  for GIMP 2.4
; Original author: Mark Lowry
;
; Tags: photo, exposure
;
; Author statement:
;
; A GIMP script-fu to perform the technique known as
; "Local Contrast Enhancement" to remove haze and bring
; out fine details.  It uses the Unsharp Mask plug-in,
; with a large radius and a small amount.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; Created on 4/15/2006 for 2.2.8
; Revised on 10/27/2007 to change menu registry location.  Tested on v.2.4.0
; Revised on 10/31/2007 to change menu location to FX-Foundry
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




(define (script-fu-LCE  img drawable inRadius inAmount )

   ; Start an undo group.  Everything between the start and the end
   ; will be carried out if an undo command is issued.

   (gimp-image-undo-group-start img)

   ;; Convert the amount to a percentage ;;

   (set! inAmount (/ inAmount 100))

   ;; Call USM plug-in and run it.

   (plug-in-unsharp-mask 1 img drawable inRadius inAmount 0)

   ; Complete the undo group

   (gimp-image-undo-group-end img)

   ; Flush the display

   (gimp-displays-flush)

)


(script-fu-register "script-fu-LCE"

      "<Image>/FX-Foundry/Photo/Enhancement/Local Contrast Enhancement"

      "Runs USM plug-in to remove haze/enhance details.  Since this is used frequently, it's nice to have so that the standard values are always available for use."

      "Mark Lowry"

      "Technique discussed on the Luminous Landscape website"

      "2007"

      "RGB*, GRAY*"

      SF-IMAGE "Image" 0

      SF-DRAWABLE "Current Layer" 0

      SF-ADJUSTMENT "Radius?"  '(50 25 75 1 10 0 0)

      SF-ADJUSTMENT "Amount?"  '(12 0 100 1 10 0 0)

 )


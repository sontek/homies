;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Copy Visible & Paste as Layer script  for GIMP 2.4
; Created by Art Wade
;
; Tags: tool
;
; Author statement:
;
; This script copies the visible layers contained within the active
; image and pastes the results into a single layer at the top of the stack
; of the active image  It is an updated version of Karl Ward's merge-copy script
; which can be found here: http://kward1979uk.deviantart.com/art/Merge-copy-28607825
;
; The Script can be found in the Image's Edit Menu under the name: "Copy Visible & Paste"
;
; Special thanks to saulgoode at gimptalk.com for his suggestions on improving the script.
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




; Define the Function

(define
    (script-fu-copy-visible-paste
    inImage
    inDraw)

; Declare the Variables

    (let*

        (
            (theHeight (car (gimp-image-height inImage))) ; returns the image height.
            (theWidth (car (gimp-image-width inImage))) ; returns the image width.
            (theFloatingSelection 0) ; an undefined variable to be defined below.
            (theType (car (gimp-drawable-type-with-alpha inDraw))) ; returns the drawables' type with an Alpha Channel
            (theSelection 0) ; undefined variable to contain the selected area in the image.
            (theResult 0) ; undefined variable created below to hold the contents of the Copied Layers
        )

; Begin Undo Group

(gimp-undo-push-group-start inImage)

; Get the selected area in the image and save it to a channel

(set! theSelection (car (gimp-selection-save inImage)))

; Clear the selection

(gimp-selection-none inImage)

; Copies the visible layers in the active image

(gimp-edit-copy-visible inImage)

; Creates a new layer which will ultimately contain the Copied Layers and gives it the name "Copied Layers".

(set! theResult (car (gimp-layer-new inImage theWidth theHeight theType "Copied Layers" 100 NORMAL-MODE)))

; Fills theResult Layer (aka Copied Layers) with transparent fill

(gimp-drawable-fill theResult TRANSPARENT-FILL)

; Adds theResult Layer to the active image

(gimp-image-add-layer inImage theResult 0)

; Defines theFloatingSelection variable as containing the pasted layers and keeping the selection active

(set! theFloatingSelection (car (gimp-edit-paste theResult FALSE)))

; Anchors theFloatingSelection to theResult

(gimp-floating-sel-anchor theFloatingSelection)

; Reload the original selection (theSelection)

(gimp-selection-load theSelection)

; Delete the channel holding the original selection (theSelection)

(gimp-image-remove-channel inImage theSelection)

; End Undo Group

(gimp-undo-push-group-end inImage)

)

; Update display

(gimp-displays-flush)

)
(script-fu-register     "script-fu-copy-visible-paste"
            "<Image>/Edit/Copy/Copy Visible & Paste as Layer"
            "Copies the visable layers to a new layer and pastes them in into active image"
            "Art Wade"
            "Art Wade"
            "December 2007"
            ""
            SF-IMAGE      "SF-IMAGE" 0
            SF-DRAWABLE   "SF-DRAWABLE" 0

)
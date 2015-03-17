;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Sepoina Graf-ix decor-filter script  for GIMP 2.4
; Original author: Ghigi Giancarlo (software@sepoina.it)
; translated By Patty
;
; Tags: photo, artistic
;
;
; Author statement:
; Variables:
; Equalize            equalize        1/0
; Spread            spreading        1/0
; Pre-sharpen             presharpen        0-99
; Smart blur            smartblur        0-30
; Engraving mask        engravareas        0-8
; Post-sharpen            postsharpen        0-99
; Canvas            texture            0-10
; Background Pattern         background        "text"
; Basic paper colour        paper            "colour"
; Output type                        0-10
;
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; last version at: www.sepoina.it/grafix/index.htm
; this.version: 1.03
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


(define (script-fu-SepoinaGrafix inImage inLayer equalizza propagazione preaffila sfocaintelligente incisaree postaffila tela fondo carta tipo)
 (let* (
         (LayerBase 0)
         (LayerSobel 0)
         (LayerSemi 0)
         (maschera 0)
         (LayerPieno 0)
         (LayerCarta 0)
         (posterizzazione 0)
         (conta 0)
       )

  (gimp-image-undo-group-start inImage)            ; Prepare any undo
  (gimp-selection-all inImage)                ; Select whole image
  (set! LayerBase (car(gimp-image-flatten inImage)))    ; Set the Layer to whole image flattened on one level
  (gimp-drawable-set-name LayerBase "Base")        ; the name of LayerBase picture is Base


  ;
  ;   Crea il piano LayerSobel
  ;
  (set! LayerSobel
   (car (gimp-layer-copy LayerBase TRUE)))        ; Copy current layer into "LayerSobel"
  (gimp-image-add-layer inImage LayerSobel -1)        ; New layer at the top of layers
  (gimp-drawable-set-name LayerSobel "Sobel")        ; picture plan's name "Sobel"
  (if (> preaffila 0)
   (plug-in-sharpen TRUE inImage LayerSobel preaffila))    ; Pre-sharp
  (if (> sfocaintelligente 0)
   (plug-in-sel-gauss
    TRUE inImage LayerSobel sfocaintelligente 60))    ; smartly blur image
  (if (= equalizza TRUE)
   (plug-in-normalize TRUE inImage LayerSobel))      ; Equalize image
  (if (> incisaree 0)
   (plug-in-unsharp-mask
    TRUE inImage LayerSobel 3.3 incisaree 29))        ; adjacent areas blurer
  (if (> postaffila 0)
   (plug-in-sharpen
    TRUE inImage LayerSobel postaffila))        ; Post-sharp
  (gimp-brightness-contrast LayerSobel 60 32)        ; Change Contrast
  (plug-in-laplace FALSE inImage LayerSobel)        ; Find contours
  (gimp-invert LayerSobel)                ; Invert
  (gimp-desaturate LayerSobel)                ; Desature
  ;(gimp-image-convert-grayscale inImage)        ; Turn to grayscale
  (if (> tela 0)
   (plug-in-apply-canvas
    TRUE inImage LayerSobel 0 tela))            ; canvas?
  (gimp-brightness-contrast LayerSobel 0 -20)          ; Uncontrast
  (gimp-brightness-contrast LayerSobel -62 86)      ; Uncontrast


  ;
  ;    Make scratched levels
  ;

  (set! LayerSemi
   (car (gimp-layer-copy LayerBase TRUE)))        ; Copy basic layer into New
  (gimp-image-add-layer inImage LayerSemi 0)        ; New layer at the top of layers
  (plug-in-sharpen TRUE inImage LayerSemi 82)         ; sharp
  (gimp-brightness-contrast LayerSemi 71 0)        ; hyperlight
  (gimp-desaturate LayerSemi)                ; Make gray
  ;(plug-in-c-astretch TRUE inImage LayerSemi)        ; Spread contrast to whole scale
  (plug-in-normalize TRUE inImage LayerSemi)        ; Spread contrast to whole scale
  (set! maschera
   (car (gimp-layer-create-mask LayerSemi 5)))        ; Create a mask based on current layer's gray copy
  (gimp-layer-add-mask LayerSemi maschera)        ; Apply trasparency mask to current layer
  (gimp-layer-remove-mask LayerSemi MASK-APPLY)        ; load mask into layer
  (gimp-drawable-set-name LayerSemi "semi")        ; new layer's name is "semi"
  (set! LayerPieno
   (car
    (gimp-layer-new-from-drawable LayerBase inImage)))        ; Crete a new layer
  (gimp-drawable-set-name LayerPieno "Pieno")             ; layer PIENO's name is "PIENO"
  (gimp-image-add-layer inImage LayerPieno 10)            ; Nuovo layer in coda ai layer
  (gimp-context-set-pattern fondo)                ; "background" is the new filling up style
  (gimp-drawable-fill LayerPieno 4)                ; Fill up layer pieno with this filling

  ;
  ;  Paper plane
  ;
  (set! LayerCarta
   (car (gimp-layer-copy LayerBase TRUE)))            ; Copy basic layer into Paper
  (gimp-image-add-layer inImage LayerCarta 100)            ; Add layer at the end
  (gimp-context-set-background carta)                ; prepare paper colour


 ; Modalit� piani                Method plans
 ; 0 = Normale                   0 = Normal
 ; 1 = Dissolvenza               1 = Fade out
 ; 3 = Multipla                  3 = Multiple
 ; 4 = Screen                    4 = Screen
 ; 5 = Sovrapposta               5 = Superimposed
 ; 6 = Differenza                6 = Difference
 ; 7 = Aggiunta                  7 = Sum
 ; 8 = Sottrai                   8 = Take away
 ; 9 = Solo toni scuri           9 = Only dark shades
 ; 10 = solo toni chiari         10 = only light shades
 ; 11 = tonalit�                 11 = shade
 ; 12 = saturazione              12 = saturation
 ; 13 = colore                   13 = colour
 ; 14 = valore                   14 = value
 ; 15 = divisione                15 = division
 ; 16 = scherma                  16 = screen
 ; 17 = brucia                   17 = burn
 ; 18 = luce forte               18 = bright light
 ; 19 = luce debole              19 = faint light
 ; 20 = estrazione grani         20 = enphasize grain
 ; 21 = fusione grani            21 = blend grain


 (set! posterizzazione FALSE)                ; only some filters posterize the result
 (gimp-image-raise-layer-to-top inImage LayerSobel)    ; Put sobel layer at the top

 ;
 ; Tipologie di output
 ;

  (if (= tipo 0)                 ;Zaza
   (Zaza LayerSobel    17     100
       LayerSemi    0    0
       LayerBase    0    51
       LayerPieno    0    100
       LayerCarta    0    0))

  (if (= tipo 1)                 ;LSD
   (Zaza LayerSobel    17     100
       LayerSemi    0    20
       LayerBase    13    100
       LayerPieno    0    100
       LayerCarta    0    0))

  (if (= tipo 2)                 ;Matite acquarellate
   (Zaza LayerSobel    17     100
       LayerSemi    18    87
       LayerBase    13    100
       LayerPieno    0    100
       LayerCarta    0    0))

  (if (= tipo 3)                 ;Pastelli graffiati
   (Zaza LayerSobel    17     100
       LayerSemi    18    87
       LayerBase    18    100
       LayerPieno    0    100
       LayerCarta    0    0))

  (if (= tipo 4)                 ;Matite scolorate
   (Zaza LayerSobel    3     100
       LayerSemi    18    58
       LayerBase    18    16
       LayerPieno    0    100
       LayerCarta    0    0))

  (if (= tipo 5)                 ;Yoga
   (Zaza LayerSobel    3     100
       LayerSemi    19    48
       LayerBase    18    0
       LayerPieno    0    50
       LayerCarta    0    100))

  (if (= tipo 6)                 ;bn penna matita carta bianca
   (begin
    (Zaza LayerSobel    3     100
       LayerSemi    18    54
       LayerBase    3    0
       LayerPieno    0    56
       LayerCarta    0    100)
    (gimp-context-set-background '(255 255 255))
   ))

  (if (= tipo 7)                 ;bn penna carta gessetto
   (Zaza LayerSobel    3     100
       LayerSemi    18    54
       LayerBase    0    0
       LayerPieno    0    54
       LayerCarta    0    100))

  (if (= tipo 8)                 ;PsicoPaint
   (Zaza LayerSobel    3     81
       LayerSemi    9    28
       LayerBase    16    70
       LayerPieno    0    0
       LayerCarta    0    100))

  (if (= tipo 9)                 ;Acquarelguson
   (begin
    (Zaza LayerSobel    17     87
        LayerSemi    0    0
        LayerBase    13    100
        LayerPieno    0    0
        LayerCarta    0    0)
    (set! posterizzazione TRUE)
    (set! propagazione TRUE)
   ))

  (if (= tipo 10)                 ;Acquarello Faber
   (begin
    (Zaza LayerSobel    3     34
        LayerSemi    0    0
        LayerBase    18    78
        LayerPieno    0    0
        LayerCarta    0    100)
    (set! propagazione TRUE)
   ))


 ;
 ;  Spread background colour to soften
 ;
  (if (= propagazione TRUE)
   (begin
     (set! conta 0)
     (while (< conta 30)
       (plug-in-vpropagate TRUE inImage LayerBase 2 255 1 15 0 255)
       (set! conta (+ conta 1))
     )
   )
  )



 ;
 ;  Posterization
 ;
  (if (= posterizzazione TRUE)
   (gimp-posterize LayerBase 50)
  )



 ;
 ;  Paint paper
 ;
  (gimp-edit-fill LayerCarta BACKGROUND-FILL)        ; Colora il layer
  (gimp-drawable-set-name LayerCarta "Carta")         ; il nome del layer Nuovo � "semi"

 ;
 ; Finali
 ;
 ;(set! LayerBase (car(gimp-image-flatten inImage)))      ; Setta theLayer a tutta l'immagine appiattita su un unico livello
 ;Scolpisce
 ; (if (= scolpisce TRUE)
 ;    (script-fu-carve-it inImage LayerBase LayerBase TRUE))

 ;
 ;   Close
 ;
  (gimp-selection-none inImage)              ;Unselect
  (gimp-image-undo-group-end inImage)            ;Any Undo
  (gimp-displays-flush inImage)             ;Re-visualize
 )
)


;
;  set all plans depending on selection
;
(define (Zaza a1 a2 a3 b1 b2 b3 c1 c2 c3 d1 d2 d3 e1 e2 e3)
  (gimp-layer-set-mode a1 a2)                    ; a2 mode for a1
  (gimp-layer-set-opacity a1 a3)            ;  dull a3 for a1
  (gimp-layer-set-mode b1 b2)                    ;
  (gimp-layer-set-opacity b1 b3)            ;
  (gimp-layer-set-mode c1 c2)                    ;
  (gimp-layer-set-opacity c1 c3)            ;
  (gimp-layer-set-mode d1 d2)                    ;
  (gimp-layer-set-opacity d1 d3)            ;
  (gimp-layer-set-mode e1 e2)                    ;
  (gimp-layer-set-opacity e1 e3)            ;
  (if (= a3 0)   (gimp-drawable-set-visible a1 0)  )    ; unset layer if it has any effect
  (if (= b3 0)   (gimp-drawable-set-visible b1 0)  )    ; "
  (if (= c3 0)   (gimp-drawable-set-visible c1 0)  )    ; "
  (if (= d3 0)   (gimp-drawable-set-visible d1 0)  )    ; "
  (if (= e3 0)   (gimp-drawable-set-visible e1 0)  )    ; "
)



;
; Register the function with the GIMP:
;
(script-fu-register
"script-fu-SepoinaGrafix"
_"<Image>/FX-Foundry/Artistic/Sepoina Graf-ix..."
"Alter a picture into a scratched image or a watercoloured paint.
Full details, demostrative examples and any new versions on...

http://www.sepoina.it/grafix/index.htm

If you find other levels merges output types
send it to me at software@sepoina.it !
Same address for Bug!
"
"Ghigi Giancarlo - software@sepoina.it"
"Ghigi Giancarlo 2004, Italy."
"16th April 2004"
"RGB*"
SF-IMAGE      "The Image"     0
SF-DRAWABLE   "The Layer"     0
SF-TOGGLE   _"Equalize" FALSE
SF-TOGGLE   _"Spreading (slow)" FALSE
SF-ADJUSTMENT _"Pre-sharp (0=No)"  '(70 0 99 0.05 0.5 2 0)
SF-ADJUSTMENT _"Smart-blur (0=NO)"  '(1.5 0 30 0.5 1 2 0)
SF-ADJUSTMENT _"Engraving mask (0=No)"  '(5 0 8 0.05 0.5 2 0)
SF-ADJUSTMENT _"Post-sharp (0=No)"  '(12 0 99 0.05 0.5 2 0)
SF-ADJUSTMENT _"Canvasize (0=NO)"  '(0 0 10 1 1 2 0)
SF-PATTERN _"Scratched texture" "Paper"
SF-COLOR   _"Paper Color" '(159 122 43)
SF-OPTION    _"Output type"         '(_"Zaza"
                       _"LSD"
                       _"Watercoloured pencils"
                       _"Scratched pencils"
                       _"Uncoloured pencils"
                       _"Yoga"
                       _"BW pen pencil white paper"
                       _"BW pen pencil chalk coloured paper"
                       _"PsicoPaint"
                       _"Acquarelguson (slow)"
                       _"Watercolour Faber (slow)"
                       )
)

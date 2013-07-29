; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Advanced Photo LAB Color Punch script  for GIMP 2.4
; Copyright (C) 2005 Lasm <lasm@rocketmail.com>
;  http://www.godsimmediatecontact.com
;  http://www.godsdirectcontact.org
;  http://www.raindesigninc.com
;
; Tags: color
;
;  Latest scripts/packages available at
;  http://sourceforge.net/projects/lasmz/
;  http://groups.yahoo.com/group/script-fu/
;
; Welcome to the Line Art Coffee House
; This Photo LAB Color Punch script is for kung-fu connisseurs only
; If it doesn't work for your images, perhaps you prefer the Bubble Tea House next door ?
; lab-color-punch.scm - lasm's famous Photo LAB Color Punch script a.k.a. Grand Mother's Kung-fu script
; Dedication - to my mother (1917-2002)
;
; --------------------------------------------------------------------
; version 1.0  by Lasm 2005/01/11 <lasm@rocketmail.com>
;     - Initial pre-release
; version 2.0  by Lasm 2005/03/11 <lasm@rocketmail.com>
;     - Added de-saturate option
; version 2.1  by Lasm 2005/03/11 <lasm@rocketmail.com>
;     - Added color-tint option
; version 2.2  by Lasm 2005/03/11 <lasm@rocketmail.com>
;     - color-tint for all channels
; version 3.0  by Lasm 2005/15/11 <lasm@rocketmail.com>
;     - rework UI elements
;     - layer name changed
;     - add curve strength for all channels
;     - add B-channel lock
; version 3.1  by Lasm 2005/20/11 <lasm@rocketmail.com>
;     - added to Sourceforge/CVS
; version 3.2  by Lasm 2005/22/11 <lasm@rocketmail.com>
;     - admended layer name
; version 4.0  by Lasm 2005/24/11 <lasm@rocketmail.com>
;     - addded highlights/shadows masking effect
; version 4.1  by Lasm 2005/30/11 <lasm@rocketmail.com>
;     - minor adjustment to layer name and script-fu constants
;
; --------------------------------------------------------------------
; You are free to use/distribute this program provided the
; comments are left intact.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Instructions on using this script
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Copy this script in the script directory.
; 2. Open up your favourite color photo in Gimp.
; 3. Look for it under Script-Fu->Lasm's FX Effects and fire away !
; 4. Put the Lab Color Punch layer above the original
; 5. Adjust opacity to taste !
;
; That's all folks. Have fun with this script !
; Another Grandmother Coffee House production.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Options Notes
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. H-Channel
;    H-Channel allows you to fine-tune the Saturate/Desaturate/Color Tint
;    operation to be carried out in parts of the image that corresponds
;    to the image's highlight/shadow area.
; 2. H-Channel sensitivity allows you to enlarge/shrink the portion of the image
;    which are designated as highlight or shadow regions.
; 3. Increasing sensitivity selects more highlight/shadow regions.
; 4. Decreasing sensitivity shrinks the highlight/shadow regions.
;; Set register common information
(define SCRIPT-FU-LAB-COLOR-PUNCH-CHOICE1 (list "Saturate" "Desaturate" "Color Tint" "Hold"))
(define SCRIPT-FU-LAB-COLOR-PUNCH-CHOICE2 (list "Hold" "Saturate" "Desaturate" "Color Tint"))
(define SCRIPT-FU-LAB-COLOR-PUNCH-CHOICE3 (list "Hold" "Highlight" "Shadow"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Advanced Photo LAB Color Punch function:
;
; Requires:
;   plug-in-decompose
;   plug-in-drawable-compose
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-lab-color-punch img inLayer Highsdw hisense Achannel curvestr1 lockB? Bchannel curvestr2 Lchannel curvestr3 newlayer?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Helper function to create layer names
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define (make-layer-name Hchannel Hisense Achannel curvestr1 lockB? Bchannel curvestr2 Lchannel curvestr3)
   (define wuji1 (number->string curvestr1 10))
   (define wuji2 (number->string curvestr2 10))
   (define wuji3 (number->string curvestr3 10))
   (define wuji4 (number->string Hisense 10))
   (string-append
    "LAB Color Punch "
    (if (= stat-H 1)
      (cond
      ((= Hchannel 0) (string-append "" ""))
      ((= Hchannel 1) (string-append wuji4 " Highlight "))
      ((= Hchannel 2) (string-append wuji4 " Shadow ")))
      "")
    (cond
      ((= Achannel 0) (string-append wuji1 " A-Saturate "))
      ((= Achannel 1) (string-append wuji1 " A-Desaturate "))
      ((= Achannel 2) (string-append wuji1 " A-Tint "))
      ((= Achannel 3) ""))
    (if (eqv? lockB? TRUE)
      "B-locked "
        (begin
        (cond
          ((= Bchannel 0) (string-append wuji2 " B-Saturate "))
          ((= Bchannel 1) (string-append wuji2 " B-Desaturate "))
          ((= Bchannel 2) (string-append wuji2 " B-Tint "))
          ((= Bchannel 3) ""))))
    (cond
      ((= Lchannel 0) "")
      ((= Lchannel 1) (string-append wuji3 " L-Saturate"))
      ((= Lchannel 2) (string-append wuji3 " L-Desaturate"))
      ((= Lchannel 3) (string-append wuji3 " L-Tint")))
    )
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Helper function to return curves array
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (curve0 wuji)
  (let* ((curve-value (cons-array 8 'byte)))
   (aset curve-value 0 0)
   (aset curve-value 1 0)
   (aset curve-value 2 wuji)
   (aset curve-value 3 0)
   (aset curve-value 4 (- 255 wuji))
   (aset curve-value 5 255)
   (aset curve-value 6 255)
   (aset curve-value 7 255)
   curve-value     ; return the curve
   )
)

(define (curve1 wuji)
  (let* ((curve-value (cons-array 8 'byte)))
   (aset curve-value 0 0)
   (aset curve-value 1 0)
   (aset curve-value 2 0)
   (aset curve-value 3 wuji)
   (aset curve-value 4 255)
   (aset curve-value 5 (- 255 wuji))
   (aset curve-value 6 255)
   (aset curve-value 7 255)
   curve-value     ; return the curve
   )
)

(define (get-lab-curves saturate? curvestr)
  (if (eqv? saturate? TRUE)
    (curve0 curvestr)
    (curve1 curvestr)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Helper function to indicate which channels needs Highlight/Shadow modification
;  Returns : A list of H-A-B-L
;            H=1 => H Channel turned on (highlight/shadow)
;            H=0 => H Channel turned off
;            A/B/L = 0 => A/B/L Channel turned off
;            A/B/L = 1 => A/B/L Channel Saturate/Destaruate turned on
;            A/B/L = 2 => A/B/L Channel Color tint turned on
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (check-stats Hchl Achl lckB? Bchl Lchl)
    (define (L-list Lc)
      (lambda ()
        (if (and (> Lc 0)     ;; sat/desat
            (< Lc 3))
          (cons '1 '())
          (if (= Lc 3)        ;; color-tint
            (cons '2 '())
            (cons '0 '()))
          )
      )
    )
    (define (B-list Bc Lc)
      (lambda ()
        (if (< Bc 2)                       ;; sat/desat
          (append '(1) ((L-list Lc)))
          (if (= Bc 2)                       ;; color-tint
            (append '(2) ((L-list Lc)))
              (append '(0) ((L-list Lc))))
          )
      )
    )
    (define (B-locked Ac Lc)
      (lambda ()
        (if (< Ac 2)                        ;; sat/desat
          (append '(1) ((L-list Lc)))
          (if (= Ac 2)                        ;; color-tint
            (append '(2) ((L-list Lc)))
            (append '(0) ((L-list Lc))))
          )
      )
    )
    (define (A-list Ac lB? Bc Lc)
      (lambda ()
        (if (eqv? lB? TRUE)
          (begin
            (if (< Ac 2)                ;; sat and desat
              (append '(1) ((B-locked Ac Lc)))
              (if (= Ac 2)                ;; color-tint
                (append '(2) ((B-locked Ac Lc)))
                (append '(0) ((B-locked Ac Lc)))))
            )
          (begin                                ;; B is un-locked
            (if (< Ac 2)                ;; sat and desat
              (append '(1) ((B-list Bc Lc)))
                (if (= Ac 2)                ;; color-tint
                (append '(2) ((B-list Bc Lc)))
                (append '(0) ((B-list Bc Lc)))))))  ;;; A-B-L
      )
    )
    (define (read-list ls)  ;; cumulative sum of list
        (if (null? ls)
          0
          (+ (car ls)
             (read-list (cdr ls))))
    )
;    ((A-list Achl lckB? Bchl Lchl))
    (if (> Hchl 0)
      (if (> (read-list ((A-list Achl lckB? Bchl Lchl))) 0)
          (append '(1) ((A-list Achl lckB? Bchl Lchl))) ;;; H-A-B-L
          (append '(0) ((A-list Achl lckB? Bchl Lchl)))) ;;; H-A-B-L
      (append '(0) ((A-list Achl lckB? Bchl Lchl)))) ;;; H-A-B-L
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Helper function to return color Tint
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (calc-color-tint curvestr)
  (let* ((color-tint (* 2 curvestr)))
  color-tint
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Helper function to create a new layer
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (copylayer layer layername)
  (let* ((new (car(gimp-layer-copy layer 1)))) ; Add an alpha channel
    (gimp-drawable-set-name new layername)
    new
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Main function
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;some globals
          (define stat-H)
          (define stat-A)
          (define stat-B)
          (define stat-L)
  (let*
    (
      (width  (car (gimp-drawable-width inLayer)))
      (height (car (gimp-drawable-height inLayer)))
      (old-fg (car (gimp-context-get-foreground)))
      (old-bg (car (gimp-context-get-background)))
      (colorTint1 (calc-color-tint curvestr1))
      (colorTint2 (calc-color-tint curvestr2))
      (colorTint3 (calc-color-tint curvestr3))
          (gray-img 0)
          (B-layer 0)
          (A-layer 0)
          (L-layer 0)
          (hisdw-img 0)
          (hisdw-color-layer 0)
          (hisdw-A-layer 0)
          (hisdw-A-mask 0)
          (hisdw-B-layer 0)
          (hisdw-B-mask 0)
          (hisdw-L-layer 0)
          (hisdw-L-mask 0)
          (hisdw-maskL 0)
          (hisdw-maskA 0)
          (hisdw-maskB 0)
          (white-layerA 0)
          (white-layerB 0)
          (white-layer2 0)
          (white-layer3 0)
          (hisdw-tintA-mask 0)
          (hisdw-tmaskA 0)
          (hisdw-tintB-mask 0)
          (hisdw-tmaskB 0)
          (hisdw-tintL-mask 0)
          (hisdw-tmaskL 0)
          (comp-img 0)
          (tt-d 0)
          (last-layer 0)
          (final-layer 0)
    )
    (gimp-image-undo-group-start img)

;; Real work goes in here

;    (gimp-image-set-active-layer img inLayer)

    (set! gray-img (car (plug-in-decompose 1 img inLayer "Lab" TRUE)))
    (set! B-layer (car (gimp-image-get-active-layer gray-img)))
    (set! A-layer (- B-layer 1))
    (set! L-layer (- A-layer 1))

    (set! stat-H (car (check-stats Highsdw Achannel lockB? Bchannel Lchannel)))
    (set! stat-A (cadr (check-stats Highsdw Achannel lockB? Bchannel Lchannel)))
      (set! stat-B (caddr (check-stats Highsdw Achannel lockB? Bchannel Lchannel)))
    (set! stat-L (car (cdddr (check-stats Highsdw Achannel lockB? Bchannel Lchannel))))
;      (gimp-message (string-append
;                      (number->string stat-H)
;                      (number->string stat-A)
;                      (number->string stat-B)
;                      (number->string stat-L)))

    (if (= stat-H 1)  ;; start of H channel create mask magic
      (begin
      (set! hisdw-img (car (gimp-image-new width height 0)))
      (set! hisdw-color-layer (car (gimp-layer-new-from-drawable inLayer hisdw-img)))
      (gimp-image-add-layer hisdw-img hisdw-color-layer -1)
      (if (= Highsdw 1)
        (gimp-threshold hisdw-color-layer 0 (- 255 (calc-color-tint hisense)))
        (if (= Highsdw 2)
          (gimp-threshold hisdw-color-layer (calc-color-tint hisense) 255)))
      (gimp-edit-copy hisdw-color-layer)
      (if (= stat-A 1)  ;; create A mask and apply it
        (begin
        (gimp-image-set-active-layer gray-img A-layer)
        (set! hisdw-A-layer (copylayer A-layer "A copy"))
        (gimp-image-add-layer gray-img hisdw-A-layer -1)
        (set! hisdw-A-mask (car (gimp-layer-create-mask hisdw-A-layer 0)))
        (gimp-layer-add-mask hisdw-A-layer hisdw-A-mask)
        (set! hisdw-maskA (car (gimp-edit-paste hisdw-A-mask TRUE)))
        (gimp-floating-sel-anchor hisdw-maskA)
        (gimp-layer-set-apply-mask hisdw-A-layer hisdw-maskA)
        ))
      (if (= stat-B 1)  ;; create B mask and apply it
        (begin
        (gimp-image-set-active-layer gray-img B-layer)
        (set! hisdw-B-layer (copylayer B-layer "B copy"))
        (gimp-image-add-layer gray-img hisdw-B-layer -1)
        (set! hisdw-B-mask (car (gimp-layer-create-mask hisdw-B-layer 0)))
        (gimp-layer-add-mask hisdw-B-layer hisdw-B-mask)
        (set! hisdw-maskB (car (gimp-edit-paste hisdw-B-mask TRUE)))
        (gimp-floating-sel-anchor hisdw-maskB)
        (gimp-layer-set-apply-mask hisdw-B-layer hisdw-maskB)
        ))
      (if (= stat-L 1)  ;; create L mask and apply it
        (begin
        (gimp-image-set-active-layer gray-img L-layer)
        (set! hisdw-L-layer (copylayer L-layer "L copy"))
        (gimp-image-add-layer gray-img hisdw-L-layer -1)
        (set! hisdw-L-mask (car (gimp-layer-create-mask hisdw-L-layer 0)))
        (gimp-layer-add-mask hisdw-L-layer hisdw-L-mask)
        (set! hisdw-maskL (car (gimp-edit-paste hisdw-L-mask TRUE)))
        (gimp-floating-sel-anchor hisdw-maskL)
        (gimp-layer-set-apply-mask hisdw-L-layer hisdw-maskL)
        ))
      ))   ;; end of H channel create mask magic

    (if (= Achannel 0)
      (begin
      (gimp-curves-spline A-layer 0 8 (get-lab-curves TRUE curvestr1))
      (if (eqv? lockB? TRUE)
        (gimp-curves-spline B-layer 0 8 (get-lab-curves TRUE curvestr1)))
      ))

    (if (= Achannel 1)
        (begin
          (gimp-curves-spline A-layer 0 8 (get-lab-curves FALSE curvestr1))
        (if (eqv? lockB? TRUE)
              (gimp-curves-spline B-layer 0 8 (get-lab-curves FALSE curvestr1)))
          ))

    (if (= stat-H 1)
      (begin
      (if (= stat-A 1)  ;; flatten A-layer
        (begin
        (gimp-image-set-active-layer gray-img hisdw-A-layer)
        (set! A-layer (car (gimp-image-merge-down gray-img hisdw-A-layer 0)))
        ))
      ))

    (if (= Achannel 2) ;; start of A tint
      (begin
      (gimp-image-set-active-layer gray-img A-layer)
      (set! white-layerA (car (gimp-layer-copy A-layer 1)))
          (gimp-image-add-layer gray-img white-layerA -1)
      (gimp-context-set-background (list colorTint1 colorTint1 colorTint1))
      (gimp-edit-fill white-layerA BG-IMAGE-FILL)

      (if (= stat-H 1)
        (begin            ;; create tint mask for A
        (set! hisdw-tintA-mask (car (gimp-layer-create-mask white-layerA 0)))
        (gimp-layer-add-mask white-layerA hisdw-tintA-mask)
        (set! hisdw-tmaskA (car (gimp-edit-paste hisdw-tintA-mask TRUE)))
        (gimp-invert hisdw-tmaskA) ;; need to invert because effect layer should be below mask layer
        (gimp-floating-sel-anchor hisdw-tmaskA)
        (gimp-layer-set-apply-mask white-layerA hisdw-tmaskA)
        )
        (begin
        (set! A-layer white-layerA)
        ))

      (if (eqv? lockB? TRUE)
        (begin
        (if (= stat-H 1)
          (begin            ;; create tint mask for B
          (gimp-image-set-active-layer gray-img B-layer)
          (set! white-layerB (car (gimp-layer-copy white-layerA 1)))
          (gimp-image-add-layer gray-img white-layerB -1)
          )
          (begin
          (set! B-layer white-layerA)
          ))
        ))  ;; end of lockB?
        (if (= stat-H 1)
        (begin            ;; flatten for A
        (gimp-image-set-active-layer gray-img white-layerA)
        (set! A-layer (car (gimp-image-merge-down gray-img white-layerA 0)))
          (if (eqv? lockB? TRUE)
          (begin            ;; flatten for B
          (gimp-image-set-active-layer gray-img white-layerB)
          (set! B-layer (car (gimp-image-merge-down gray-img white-layerB 0)))
          ))
        ))  ;; end of stat-H
      ))  ;; end of A tint


    (if (eqv? lockB? FALSE)
      (begin
      (if (= Bchannel 0)
        (gimp-curves-spline B-layer 0 8 (get-lab-curves TRUE curvestr2))
        (if (= Bchannel 1)
        (gimp-curves-spline B-layer 0 8 (get-lab-curves FALSE curvestr2))))

      (if (= Bchannel 2) ;; start of B tint
        (begin
          (gimp-image-set-active-layer gray-img B-layer)
        (set! white-layer2 (car (gimp-layer-copy B-layer 1)))
            (gimp-image-add-layer gray-img white-layer2 -1)
        (gimp-context-set-background (list colorTint2 colorTint2 colorTint2))
        (gimp-edit-fill white-layer2 BG-IMAGE-FILL)
        (if (= stat-H 1)
          (begin            ;; create tint mask for B
          (set! hisdw-tintB-mask (car (gimp-layer-create-mask white-layer2 0)))
          (gimp-layer-add-mask white-layer2 hisdw-tintB-mask)
          (set! hisdw-tmaskB (car (gimp-edit-paste hisdw-tintB-mask TRUE)))
          (gimp-invert hisdw-tmaskB) ;; need to invert because effect layer should be below mask layer
          (gimp-floating-sel-anchor hisdw-tmaskB)
          (gimp-layer-set-apply-mask white-layer2 hisdw-tmaskB)
          )
          (begin
          (set! B-layer white-layer2)
          ))
        ))  ;; end of B tint
      ))  ;; end of B unlocked

    (if (= stat-H 1)
      (begin
        (if (= stat-B 1) ;; flatten B-layer
        (begin
        (gimp-image-set-active-layer gray-img hisdw-B-layer)
        (set! B-layer (car (gimp-image-merge-down gray-img hisdw-B-layer 0)))
        ))
      (if (= stat-B 2) ;; flatten B-layer
        (begin
        (gimp-image-set-active-layer gray-img white-layer2)
        (set! B-layer (car (gimp-image-merge-down gray-img white-layer2 0)))
        ))
      ))

    (if (= Lchannel 1)
      (gimp-curves-spline L-layer 0 8 (get-lab-curves TRUE curvestr3))
      (if (= Lchannel 2)
          (gimp-curves-spline L-layer 0 8 (get-lab-curves FALSE curvestr3))))

    (if (= stat-H 1)
      (begin
      (if (= stat-L 1) ;; flatten L-layer
        (begin
        (gimp-image-set-active-layer gray-img hisdw-L-layer)
        (set! L-layer (car (gimp-image-merge-down gray-img hisdw-L-layer 0)))
        ))
      ))

    (if (= Lchannel 3)
      (begin
      (gimp-image-set-active-layer gray-img L-layer)
      (set! white-layer3 (car (gimp-layer-copy L-layer 1)))
          (gimp-image-add-layer gray-img white-layer3 -1)
      (gimp-context-set-background (list colorTint3 colorTint3 colorTint3))
      (gimp-edit-fill white-layer3 BG-IMAGE-FILL)
      (if (= stat-H 1)
        (begin
          (set! hisdw-tintL-mask (car (gimp-layer-create-mask white-layer3 0)))
          (gimp-layer-add-mask white-layer3 hisdw-tintL-mask)
          (set! hisdw-tmaskL (car (gimp-edit-paste hisdw-tintL-mask TRUE)))
          (gimp-invert hisdw-tmaskL) ;; need to invert because effect layer should be below mask layer
          (gimp-floating-sel-anchor hisdw-tmaskL)
          (gimp-layer-set-apply-mask white-layer3 hisdw-tmaskL)
          (set! L-layer (car (gimp-image-merge-down gray-img white-layer3 0)))
          )
        (begin
        (set! L-layer white-layer3)
        ))
      ))

    (set! comp-img (car
        (plug-in-drawable-compose RUN-NONINTERACTIVE gray-img L-layer A-layer B-layer 0 "Lab")))
    (set! tt-d (car (gimp-display-new comp-img))) ;; working

;; Create layer name
        (set! last-layer (car (gimp-image-get-active-drawable comp-img)))
        (if (eqv? newlayer? TRUE)
     (begin
        (set! final-layer (car (gimp-layer-new-from-drawable last-layer img)))
            (gimp-image-add-layer img final-layer -1)
        (gimp-drawable-set-name final-layer (make-layer-name Highsdw hisense Achannel curvestr1 lockB? Bchannel curvestr2 Lchannel curvestr3))
            (gimp-display-delete tt-d))
     (begin
         (gimp-drawable-set-name last-layer (make-layer-name Highsdw hisense Achannel curvestr1 lockB? Bchannel curvestr2 Lchannel curvestr3)))
    )


;; clean up before exit
        (gimp-image-delete gray-img)
    (if (= stat-H 1)
      (gimp-image-delete hisdw-img))

    (gimp-image-undo-group-end img)
    (gimp-displays-flush)

    (gimp-context-set-background old-bg)
    (gimp-context-set-foreground old-fg)

  )

)


(script-fu-register
 "script-fu-lab-color-punch"
 _"<Image>/FX-Foundry/Color/Lasm's LAB Color Punch"
 "Lasm's famous special effect for photographs. This works on any RGB image. LAB Color Punch gives you the power and freedom to change the color you want ! Caution: it runs slowly on large images."
 "lasm"
 "Copyright 2005, lasm"
 "Nov 1, 2005"
 "RGB*"
 SF-IMAGE              "The Image"           0
 SF-DRAWABLE           "The Layer"           0
 SF-OPTION            _"Highlight/Shadow"        SCRIPT-FU-LAB-COLOR-PUNCH-CHOICE3
 SF-ADJUSTMENT        _"Sensitivity"           '(64 0 127 1 10 0 0)
 SF-OPTION            _"Green/Red"            SCRIPT-FU-LAB-COLOR-PUNCH-CHOICE1
 SF-ADJUSTMENT        _"Strength"             '(64 0 127 1 10 1 0)
 SF-TOGGLE            _"Lock Blue/Yellow"        TRUE
 SF-OPTION            _"Blue/Yellow"            SCRIPT-FU-LAB-COLOR-PUNCH-CHOICE1
 SF-ADJUSTMENT        _"Strength"             '(64 0 127 1 10 1 0)
 SF-OPTION            _"Luminosity"            SCRIPT-FU-LAB-COLOR-PUNCH-CHOICE2
 SF-ADJUSTMENT        _"Strength"             '(64 0 127 1 10 1 0)
 SF-TOGGLE            _"New Layer"             TRUE
)

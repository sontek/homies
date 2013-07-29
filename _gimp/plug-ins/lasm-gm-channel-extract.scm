; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Advanced Channel Extraction script  for GIMP 2.3.4
; Copyright (C) 2005 Lasm <lasm@rocketmail.com>
;  http://www.godsimmediatecontact.com
;  http://www.godsdirectcontact.org
;  http://www.raindesigninc.com
;
; Tags: tool, color
;
;  Latest scripts/packages available at
;  http://sourceforge.net/projects/lasmz/
;  http://groups.yahoo.com/group/script-fu/
;
; Welcome to the Line Art Coffee House
; This Channel Extract script is for coffee-connisseurs only
; If it doesn't work for your images, perhaps you prefer the Bubble Tea House next door ?
; gm-channel-extract.scm - lasm's famous Channel Extract script a.k.a. 18 scholar script
;
; --------------------------------------------------------------------
; version 1.0  by Lasm 2005/07/12 <lasm@rocketmail.com>
;     - Initial release
; version 1.1  by Lasm 2005/29/12 <lasm@rocketmail.com>
;     - Added "All" option to select all 17 channels
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
; 4. Look for it under Script-Fu->Lasm's FX Effects and fire away !
;
; That's all folks. Have fun with this script !
; Another Grandmother Coffee House production.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Channel Extractor function:
;
; Requires:
;   plug-in-decompose
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (script-fu-gm-channel-extract img inLayer All? Red? Green? Blue? L? A? B? C? M? Y? H? S? V? Cyan? Magenta? Yellow? Black? Alpha?)

; below is replacement for (lref-default list index default-fcn) on platforms greater or equal to 2.4
;Returns the index element of the list or the result of calling the default-fcn if the list is not long enough.
;David M. W. Martin (occamsrazor) 02/11/07


 (if (>= (string->number (substring (car(gimp-version)) 0 3)) 2.4)
       (define (lref-default ls index default-fn)
          (if not(list-ref ls index)  default-fn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Helper function to make a stack
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-stack            ;; generic stack template code
  (lambda ()
    (let ((ls '()) (p '()) (c 'a))
      (lambda (msg . args)
        (cond
          ((eqv? msg 'empty?) (null? ls))
          ((eqv? msg 'push!)
           (set! ls (cons (car args) ls)))
          ((eqv? msg 'top) (car ls))
          ((eqv? msg 'pop!)
           (set! c (car ls)) (set! ls (cdr ls)) c)
          ((eqv? msg 'print) (set! p ls) (set! ls '()) p)    ;for printing the value
          (else "oops"))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Helper function to process two lists as stacks and return a stack.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  It parses user input options and ouput 1 or 0 in the return stack for the
;  corresponding image type (RGB, HSV, etc) requested
;  List-L1 control stack indicates how many channels make up one image type (rgb, cmy etc)
;  List-L2 actual parameters list
;  Return Stack : 1 - user want to create this image type, 0 - skip this image type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define (get-channel-stats List-L1  List-L2)
  (let* ((count 0)
         (return-val 0)
   )
   (define (create-stack List-l stack-name) ; populates a stack with contents of a list
      (if (null? List-l)
       ()
       (begin
         (stack-name 'push! (car List-l))
         (create-stack (cdr List-l) stack-name)))
   )
  (define stck-ctl (make-stack))  ; create control stack
  (create-stack List-L1 stck-ctl) ; populate it

  (define stck-chl (make-stack))  ; create channel stack
  (create-stack List-L2 stck-chl) ; populate it

  (define stck-return (make-stack))  ; create return stack
  (while (not (stck-ctl 'empty?)) ; populate it
    (set! count (stck-ctl 'pop!))
    (set! return-val 0)
    (while (> count 0)
      (if (> (stck-chl 'pop!) 0)  ; predicate TRUE?
        (set! return-val 1))
      (set! count (- count 1))
      )
    (stck-return 'push! return-val)
  )
  stck-return
  )
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Thatagatha - Worker function to process requests
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; parameters-ls is actual options
; template-ls is unique image type signature template (rgb-i, cmy-i etc)
; 1. Order of image layers list in template-ls correspond to order of parameters-ls
; 2. The car of the list is the first image layer returned by plug-in-decompose
; 3. A new layer will be created when the corresponding option in parameters-ls is TRUE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (thatagatha parameters-ls template-ls)

  (define (add-layer fr-layer to-img layer-name)
   (let* (
          (new-layer (car (gimp-layer-new-from-drawable fr-layer to-img))))
    (gimp-image-add-layer to-img new-layer -1)
    (gimp-drawable-set-name new-layer layer-name)
   )
  )

  (define (iter1 a top curr parenti grayi lpre img-ls parms-ls)




      (if (<= a  top)
          (begin
            (if (= a 1)  ;; top most layer id from plug-in-decompose
                (set! curr (car (gimp-image-get-active-layer grayi)))
                (set! curr (- curr 1))           ;; else current and subsequent layer id is below top layer
                )
            (if (eqv? (lref-default parms-ls (- a 1) ()) TRUE)  ; parms predicate
                (add-layer curr parenti (string-append    ; create this layer
                           lpre
                           (lref-default img-ls (- a 1) ())
                           ))
                )
            (iter1 (+ a 1) top curr parenti grayi lpre img-ls parms-ls)
          )
      )
  )

  (define inImg (car template-ls))           ; parent Image
  (define inLyr (cadr template-ls))          ; parent Layer
  (define inLyrname (caddr template-ls))        ; new layer name prefix "GM"
  (define imgType (caar (cdddr template-ls))) ; list of list
  (define imgLyrs-ls (cdar (cdddr template-ls))) ; list of image layers template
  (define imgLen (length imgLyrs-ls))            ; length of image layers template = length of parameters-ls

  (define gray-img (car (plug-in-decompose RUN-NONINTERACTIVE inImg inLyr imgType TRUE)))

  (iter1 1 imgLen 0 inImg gray-img inLyrname imgLyrs-ls parameters-ls)

  ;; we're done creating layers, now remove grayscale
  (gimp-image-delete gray-img)
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Helper functions to create image template curry functions
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parent-img (list inLayer img))
(define layername-prefix "GM ")
(define (curry f args)           ;; generic curry function takes 2 args
  (lambda x
    (f x args)))
(define curry-parent (curry append parent-img))
(define curry-p (curry-parent layername-prefix))
(define curry-a (curry append curry-p))    ;; append curry function
(define rgb-i (curry-a (list "RGB" "Blue" "Green" "Red")))
(define rgb-i (reverse rgb-i))   ;; more logical ordering of arguments
(define lab-i (curry-a (list "LAB" "B" "A" "L")))
(define lab-i (reverse lab-i))
(define hsv-i (curry-a (list "HSV" "Value" "Saturation" "Hue")))
(define hsv-i (reverse hsv-i))
(define cmy-i (curry-a (list "CMY" "Yellow" "Magenta" "Cyan")))
(define cmy-i (reverse cmy-i))
(define cmyk-i (curry-a (list "CMYK" "Black" "Yellow-K" "Magenta-K" "Cyan-K")))
(define cmyk-i (reverse cmyk-i))
(define alpha-i (curry-a (list "Alpha" "Alpha")))
(define alpha-i (reverse alpha-i))
(define rgb-f (curry thatagatha rgb-i))           ;; worker curry functions for rgb type image
(define lab-f (curry thatagatha lab-i))           ;; lab image
(define cmy-f (curry thatagatha cmy-i))           ;; cmy image
(define hsv-f (curry thatagatha hsv-i))           ;; hsv image
(define cmyk-f (curry thatagatha cmyk-i))         ;; cmyk image
(define alpha-f (curry thatagatha alpha-i))       ;; alpha image
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Main function
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let*
     ((stck-ret 0)
      (stat-RGB 0)
      (stat-LAB 0)
      (stat-CMY 0)
      (stat-HSV 0)
      (stat-CMYK 0)
      (stat-Alpha 0)
     )

     (gimp-image-undo-group-start img)

;; Real work goes in here

    (gimp-image-set-active-layer img inLayer)

    (if (eqv? All? TRUE)             ;; select All options
      (begin
        (rgb-f TRUE TRUE TRUE)
        (lab-f TRUE TRUE TRUE)
        (cmy-f TRUE TRUE TRUE)
        (hsv-f TRUE TRUE TRUE)
        (cmyk-f TRUE TRUE TRUE TRUE)
        (if (> (car (gimp-drawable-has-alpha inLayer)) 0)
          (alpha-f TRUE))
      )
      (begin
        ; process options
        ; control stack indicates how many channels make up one image type
        (set! stck-ret (get-channel-stats '(3 3 3 3 4 1) (list Red? Green? Blue? L? A? B? C? M? Y?
                                                               H? S? V?
                                                               Cyan? Magenta? Yellow? Black?
                                                               Alpha?)))
        (set! stat-RGB (stck-ret 'pop!))
        (set! stat-LAB (stck-ret 'pop!))
        (set! stat-CMY (stck-ret 'pop!))
        (set! stat-HSV (stck-ret 'pop!))
        (set! stat-CMYK (stck-ret 'pop!))
        (set! stat-Alpha (stck-ret 'pop!))

        (if (= stat-RGB 1)
          (rgb-f Blue? Green? Red?))
        (if (= stat-LAB 1)
          (lab-f B? A? L?))
        (if (= stat-CMY 1)
          (cmy-f Y? M? C?))
        (if (= stat-HSV 1)
          (hsv-f V? S? H?))
        (if (= stat-CMYK 1)
          (cmyk-f Black? Yellow? Magenta? Cyan?))
        (if (= stat-Alpha 1)
          (if (> (car (gimp-drawable-has-alpha inLayer)) 0)
            (alpha-f Alpha?)))

         ))            ;; end of All options

;; clean up before exit
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)

  )

)


(script-fu-register
 "script-fu-gm-channel-extract"
 _"<Image>/FX-Foundry/Toolbox/Lasm's Channel Extract"
 "Version 1.1 \nThis utility works on any RGBA image and let you extract all or any one of the 17 greyscale channels of the image for photo-retouch, black and white conversion, creating masks and many other image processing purposes. Caution: it may run slowly on large images."
 "lasm"
 "Copyright 2005, lasm"
 "Dec 7, 2005"
 "RGB*"
 SF-IMAGE           "The Image"       0
 SF-DRAWABLE         "The Layer"       0
 SF-TOGGLE            _"All Channels"      TRUE
 SF-TOGGLE            _"Red"              FALSE
 SF-TOGGLE            _"Green"          FALSE
 SF-TOGGLE            _"Blue"              FALSE
 SF-TOGGLE            _"L"              FALSE
 SF-TOGGLE            _"A"              FALSE
 SF-TOGGLE            _"B"              FALSE
 SF-TOGGLE            _"Cyan"              FALSE
 SF-TOGGLE            _"Magenta"          FALSE
 SF-TOGGLE            _"Yellow"          FALSE
 SF-TOGGLE            _"Hue"              FALSE
 SF-TOGGLE            _"Saturation"      FALSE
 SF-TOGGLE            _"Value"          FALSE
 SF-TOGGLE            _"Cyan-K"          FALSE
 SF-TOGGLE            _"Magenta-K"      FALSE
 SF-TOGGLE            _"Yellow-K"          FALSE
 SF-TOGGLE            _"Black"          FALSE
 SF-TOGGLE            _"Alpha"          FALSE
 )


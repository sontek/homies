(script-fu-register "layer-to-selection"
	"<Image>/Select/Layer to selection"

        "Just make a layer selected. For some reason this is not built in."
        
	"(c) Kevin Brubeck Unhammer <unhammer(at)gmail.com>"
	"Published under GPL version 2"
	"March 8, 2009"
	"*"

	SF-IMAGE "Image" 0
	SF-DRAWABLE "Drawable" 0
)

(define
  (layer-to-selection
   image
   drawable)

  (gimp-undo-push-group-start image)

  ;; is there a quicker way to do this?
  (let* ((layer (car (gimp-image-get-active-layer image)))
         (width (car (gimp-drawable-width layer)))
         (height (car (gimp-drawable-height layer)))
         (posx (car (gimp-drawable-offsets layer)))
         (posy (cadr (gimp-drawable-offsets layer))))
      (gimp-rect-select image 
                        posx 
                        posy 
                        width 
                        height 
                        0 
                        FALSE 
                        0))

  (gimp-undo-push-group-end image)
  (gimp-displays-flush))

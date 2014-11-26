#lang racket
(require gigls/unsafe)

;;; File:
;;;   Final_Project.rkt
;;; Authors:
;;;   Logan Goldberg, Henry Fisher
;;; Summary:
;;;   
;;; Notes: 
;;; Citations:

; +-------+----------------------------------------------------------
; | Notes |
; +-------+

; +--------------------+---------------------------------------------
; | Primary Procedures |
; +--------------------+

(define image-series
  (lambda (n width height)
    (cond [(= n 666)
           (image-show (image-load "/home/goldberg/Pictures/samcollage.png"))]
          [(= n 420)
           (image-show (image-load "/home/goldberg/Pictures/stonecollage.png"))]
          [(= n 42)
           (image-show (image-load "/home/goldberg/Pictures/weinmancollage.png"))]
          [(= n 888)
           (image-show (image-load "/home/goldberg/Pictures/daviscollage.png"))]
          [(= n 0)
           (image-show (image-load "/home/goldberg/Pictures/walkercollage.png"))]
          [else
           (master-series n width height)]
          )))

(define master-series
  (lambda (n width height)
    (let ([fractal (fractal-image-series n width height)])
      (cond 
        [(> 10000 (image-area fractal))
         fractal]
        [(= 0 (modulo n 3))
         (magnifying-glass! fractal (magnify-inputs fractal) 5)]
        [(= 1 (modulo n 3))
         (distort! fractal (distortion-inputs fractal) (determine-aoe n) (determine-blur n))]
        [else 
         (rain-me! fractal (raindrop-inputs fractal) (determine-aoe n) (determine-blur n))])
      fractal)))

; An version of image-series that generates various portions of the
; fractal set.  Not documented with the six P's because that's
; part of the project, and this is sample code for the project.
(define fractal-image-series
  (lambda (n width height)
    (let* (; Our default color.  Used when we hit max-recursions.
           [FRACTAL-CHOICE (car 
                            (list-ref fractal-table 
                                      (modulo (round (/ n 4)) (length fractal-table))))]
           [ASSOC-FRACTAL (lookup-attributes FRACTAL-CHOICE fractal-table)]
           [DEFAULT (irgb 0 0 0)]
           ; The horizontal offset of the center from top-left.
           ; Use -2.0 for normal.  Might be based on n.
           [HOFFSET ((list-ref ASSOC-FRACTAL 2) n)]
           ; The vertical offset of the center from top-left.
           ; Use -1.0 for normal.  Might be based on n.
           [VOFFSET ((list-ref ASSOC-FRACTAL 3) n)]
           ; The horizontal scale.  Might be based on n
           [HSCALE ((list-ref ASSOC-FRACTAL 4) HOFFSET)]
           ; The Vertical scale.  Might be based on n.
           [VSCALE ((list-ref ASSOC-FRACTAL 5) VOFFSET)])
      (let (; Convert a point in the unit plane to a complex number within
            ; some more interesting range.  That range is determined by the
            ; parameters above.
            [complicate (lambda (x y)
                          (+ (+ HOFFSET (* HSCALE x))
                             (* 0+i (+ VOFFSET (* VSCALE y)))))])
        (image-compute
         (lambda (col row)
           (let* ([c (complicate (unit-coord-x col width) 
                                 (unit-coord-y row height))]
                  [steps (steps-to-escape c ASSOC-FRACTAL)]
                  [color (if (= steps -1)
                             DEFAULT
                             (index-color (+ n steps)))])
             color))
         width height)))))

; +----------------------+-------------------------------------------
; | Algorithm Procedures |
; +----------------------+

(define dim-row
  (lambda (matrix)
    (length matrix)))

(define dim-col
  (lambda (matrix)
    (let ([mcar (car matrix)])
      (if (not (list? mcar))
          (length matrix)
          (length mcar)))))

(define chaotic-chain
  (lambda (matrix vec power)
    (let kernel ([chaotic-vec vec][counter 0])
      (if (= counter power)
          chaotic-vec
          (kernel (matrix-multiplication matrix chaotic-vec) (+ 1 counter))))))

(define matrix-multiplication ;;; Cite Stone.
  (lambda (left right)
    (let ([left-dim-row (dim-row left)]
          [right-dim-row (dim-row right)]
          [left-dim-col (dim-col left)]
          [right-dim-col (dim-col right)])
      (if (= left-dim-col right-dim-row)
          (let left-row ([left-row-cycle 0])
            (if (= left-dim-row left-row-cycle)
                null
                (cons 
                 (let right-col ([right-col-cycle 0])
                   (if (= right-dim-col right-col-cycle)
                       null
                       (cons 
                        (let vec-functional ([position 0])
                          (if (= position right-dim-row)
                              0
                              (+ (* (list-ref (list-ref left left-row-cycle) position)
                                    (list-ref (list-ref right position) right-col-cycle))
                                 (vec-functional (+ position 1)))))
                        (right-col (+ right-col-cycle 1)))))
                 (left-row (+ left-row-cycle 1)))))
          (error "Inner-dimensions do not match. Multiplication undefined.")
          ))))

(define raindrop-inputs
  (lambda (image)
    (let ([num (num-of-inputs image)])
      (let kernel ([power 0])
        (if (= power num)
            null
            (cons (chaotic-chain chaotic-matrix-4x4-1 initial-aoe-vector power) 
                  (kernel (+ power 1))))))))

(define magnify-inputs
  (lambda (image)
    (let ([num (num-of-inputs image)])
      (let kernel ([power 0])
        (if (= power num)
            null
            (cons (chaotic-chain chaotic-matrix-3x3 initial-magnify-vector power) 
                  (kernel (+ power 1))))))))

(define distortion-inputs
  (lambda (image)
    (let ([num (num-of-inputs image)])
      (let kernel ([power 0])
        (if (= power num)
            null
            (cons (chaotic-chain chaotic-matrix-4x4-2 initial-aoe-vector power) 
                  (kernel (+ power 1))))))))

(define num-of-inputs
  (lambda (image)
    (round (* 10 (log (image-area image))))))

;;; Procedure:
;;;   steps-to-escape
;;; Parameters:
;;;   complex, a complex number.
;;;   fractal, a procedure.
;;; Purpose:
;;;   Counts the number of times we can repeatedly compute fractal
;;;   before hitting max-recursions or getting a number 
;;;   that is more than 2 away from the complex origin.  If we hit
;;;   max-recursions, we return a special value (-1).
;;; Produces:
;;;   steps, an integer
;;; Preconditions:
;;;   Let f(z) be fractal.  (We use this in the postconditions.)
;;; Postconditions:
;;;   If steps >= 0, then complex "escapes" after exactly steps iterations.  That is,
;;;     distance((f^s)(complex), 0) >= 2 and for all i, 0 <= i < s, 
;;;     distance((f^i)(complex), 0) < 2.
;;;   If steps = -1, then complex does not "escape" within max-recursions. That is,
;;;     for all i, 0 <= i <= max-recursions, distance((f^i)(complex), 0) < 2.
(define steps-to-escape
  (lambda (complex-number fractal)
    (let ([proc (cadr fractal)])
      (let kernel ([z complex-number]
                   [steps 0])
        (cond
          [(>= (complex-distance-squared z 0) 4)
           steps]
          [(>= steps max-recursions)
           -1]
          [else
           (kernel (proc complex-number z) (+ steps 1))]
          )))))

; +-------------------+----------------------------------------------
; | Effect Procedures |
; +-------------------+

;;; Procedure:
;;;   add-scaled-ellipse!
;;; Parameters:
;;;   image, an image
;;;   base-layer, a positive integer
;;;   left, an integer
;;;   top, an integer
;;;   width, an integer
;;;   height, an integer
;;;   aoe, an integer
;;;   stroke?, a boolean
;;; Purpose:
;;;   To add a single ellipse to an image that takes the portion of the image
;;;   defined by left - aoe, top - aoe, width + 2aoe, height + 2aoe, and 
;;;   put that portion into an ellipse defined by left, top, width, and height.
;;;   The scaled ellipse will be stroke if stroke? is true.
;;; Produces:
;;;   [Nothing; Called for the side effect.]
;;; Preconditions:
;;;   base-layer is a layer in image
;;; Postconditions:
;;;   image now has an ellipse with the following stats:
;;;    (image-left image) = left
;;;    (image-top image) = top
;;;    (image-width image) = width
;;;    (image-height image) = height
;;;    (if stroke? (the selection will be stroked)
;;;                (the selection will not be stroked))
(define add-scaled-ellipse!
  (lambda (image base-layer left top width height aoe stroke?)
    (copy-and-add-layer! image base-layer)
    (let ([temp-layer (get-top-layer image)])
      (image-select-ellipse! image REPLACE 
                             (bound 0 (- (image-width image) 1) (- left aoe))
                             (bound 0 (- (image-height image) 1) (- top aoe))
                             (+ (* 2 aoe) width)
                             (+ (* 2 aoe) height))
      (when stroke? 
        (context-set-brush! "2. Hardness 100" 3)
        (repeat 1 (image-stroke-selection! image)))
      (scale-selection-into-new-layer!
       temp-layer left top (+ left width) (+ top height))
      (gimp-image-remove-layer image temp-layer)
      (merge-floating-layer! image (get-top-layer image) 1))))

;;; Procedure:
;;;   scale-selection-into-new-layer!
;;; Parameters:
;;;   layer, a positive integer
;;;   top-left-x, a positive integer
;;;   top-left-y, a positive integer
;;;   bot-right-x, a positive integer
;;;   bot-right-y, a positive integer
;;; Purpose:
;;;   To scale a selection on layer into a new shape on a new layer.
;;; Produces:
;;;   [Nothing; Called for the side effect.]
;;; Preconditions:
;;;   There must be a selection on the image
;;;   layer is a layer in image
;;; Postconditions:
;;;   The previous selection is now scaled into a square (though the
;;;    shape isn't necessarily a square) defined by the coordinates
;;;    top-left-x, top-left-y, bot-right-x, bot-right-y.
;;;   (get-top-layer image) contains this scaled item.
(define scale-selection-into-new-layer!
  (lambda (layer top-left-x top-left-y bot-right-x bot-right-y)
    (gimp-floating-sel-to-layer
     (car (gimp-item-transform-scale 
           layer 
           top-left-x 
           top-left-y                    
           bot-right-x 
           bot-right-y)))))

;;; Procedure:
;;;   merge-floating-layer!
;;; Parameters:
;;;   image, an image
;;;   layer-merge, a positive integer
;;;   merge-type, an integer
;;; Purpose:
;;;   To merge a layer with the layer below it.
;;; Produces:
;;;   [Nothing; Called for the side effect.]
;;; Preconditions:
;;;   merge-type must be 0, 1, or 2
;;;   layer-merge is a layer in image
;;; Postconditions:
;;;   (get-top-layer image) is the previous top two layers merged together.
(define merge-floating-layer!
  (lambda (image layer-merge merge-type)
    (gimp-image-merge-down image layer-merge merge-type)))

;;; Procedure:
;;;   blur-image
;;; Parameters:
;;;   image, an image
;;;   layer, a positive integer
;;;   repititions, a positive integer
;;; Purpose:
;;;   To blur the image.
;;; Produces:
;;;   [Nothing; Called for the side effect.]
;;; Preconditions:
;;;   layer is a layer in image
;;; Postconditions:
;;;   The image's state is equal to that of an image that has had
;;;   plug-in-blur called on it repititions times.
(define blur-image!
  (lambda (image layer repititions)
    (repeat repititions plug-in-blur 1 image layer)))

;;; Procedure:
;;;   distort!
;;; Parameters:
;;;   image, an image
;;;   distort-lst, a list
;;;   aoe, a number
;;;   blur-degree, a number
;;; Purpose:
;;;   To distort the image by creating scaled ellipses on it and then
;;;    blurring the image
;;; Produces:
;;;   [Nothing; Called for the side effect.]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   The image will look distorted.
(define distort!
  (lambda (image distort-lst aoe blur-degree)
    (render-blobs! image distort-lst
                   3
                   (round (find-biggest-distortion image))
                   aoe 0 #t)
    (blur-image! image (get-top-layer image) blur-degree)))

;;; Procedure:
;;;   magnifying-glass!
;;; Parameters:
;;;   image, an image
;;;   magnify-lst, a list
;;;   factor, a number
;;; Purpose:
;;;   To render "magnifying glasses" on the image (ellipses that magnify a 
;;;   certain area)
;;; Produces:
;;;   [Nothing; Called for the side effect.]
;;; Preconditions:
;;;   factor > 0
;;; Postconditions:
;;;   [No additional]
(define magnifying-glass!
  (lambda (image magnify-lst factor)
    (let kernel ([remaining magnify-lst])
      (if (null? remaining)
          (context-update-displays!)
          (let ([glass (car remaining)]
                [max-diameter (find-biggest-magnifier image)])
            (add-scaled-ellipse! 
             image (get-top-layer image)
             (modulo (round (car (list-ref glass 0))) (image-width image))
             (modulo (round (car (list-ref glass 1))) (image-height image))
             (+ 11 (modulo (round (car (list-ref glass 2))) max-diameter))
             (+ 11 (modulo (round (car (list-ref glass 2))) max-diameter))
             (* -1 factor) #t)
            (kernel (cdr remaining)))))))

;;; Procedure:
;;;   render-blobs!
;;; Parameters:
;;;   image, an image
;;;   lst, a list of column-vectors
;;;   min-dimension, a number
;;;   max-dimension, a number
;;;   aoe, a number
;;;   blur-degree, a number
;;;   stroke?, a boolean
;;; Purpose:
;;;   To blur the background of image, and add (length lst) scaled ellipses
;;;   to image (these ellipses are possibly outlined)
;;; Produces:
;;;   [Nothing; Called for the side effect.]
;;; Preconditions:
;;;   max-dimension > min-dimension
;;; Postconditions:
;;;   There are (length lst) scaled ellipses on image. For each ellipse e:
;;;   (> max-dimension width-of-e min-dimension)
;;;   (> max-dimension height-of-e min-dimension)
;;;   (if stroke? (each ellipse has an outline) (no ellipse has an outline))
(define render-blobs!
  (lambda (image lst min-dimension max-dimension aoe blur-degree stroke?)
    (copy-and-add-layer! image (get-top-layer image))
    (let* ([layers (cadr (gimp-image-get-layers image))]
           [base (cadr layers)]
           [blurred (car layers)])
      (blur-image! image blurred blur-degree)
      (let kernel ([remaining lst])
        (if (null? remaining)
            (context-update-displays!)
            (let* ([raw-blob (car remaining)]
                   [blob (map (o round car) raw-blob)]
                   [width (+ min-dimension (modulo (list-ref blob 2) max-dimension))]
                   [height (+ min-dimension (modulo (list-ref blob 3) max-dimension))])
              (add-scaled-ellipse!
               image base
               (modulo (list-ref blob 0) (- (image-width image) width))
               (modulo (list-ref blob 1) (- (image-height image) height))
               width height aoe stroke?)
              (kernel (cdr remaining))))))))

;;; Procedure:
;;;   rain-me!
;;; Parameters:
;;;   image, an image
;;;   raindrop-lst, a list
;;;   aoe, a number
;;;   blur-degree, a number
;;; Purpose:
;;;   To render realistic looking raindrops on an image
;;; Produces:
;;;   [Nothing; Called for the side effect.]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   Realistic rain will appear on the image, positions and lengths
;;;   based off of raindrop-lst
(define rain-me!
  (lambda (image raindrop-lst aoe blur-degree)
    (render-blobs! image raindrop-lst
                   3
                   (round (find-biggest-raindrop image))
                   aoe blur-degree #f)))

;;; Procedure:
;;;   copy-and-add-layer!
;;; Parameters:
;;;   image, an image
;;;   layer, a positive integer
;;; Purpose:
;;;   To copy layer and add that copied layer to image
;;; Produces:
;;;   [Nothing; Called for the side effect.]
;;; Preconditions:
;;;   layer must be a layer in image
;;; Postconditions:
;;;   (get-top-layer image) will be a copy of layer.
(define copy-and-add-layer!
  (lambda (image layer)
    (gimp-image-add-layer 
     image
     (car (gimp-layer-copy layer 1))
     0)))

;;; Procedure:
;;;   get-top-layer
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   To retrieve the number value of the top layer in image
;;; Produces:
;;;   layer, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   layer is the first element in the second element of 
;;;   (gimp-image-get-layers image)
(define get-top-layer
  (lambda (image)
    (caadr (gimp-image-get-layers image))))

;;; Procedure:
;;;   find-biggest-raindrop
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   To retrieve the largest width/height that any raindrop is allowed to be
;;; Produces:
;;;   n, a number
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   [No additional]
(define find-biggest-raindrop
  (lambda (image)
    (round (* 2.4 (log (image-area image))))))

;;; Procedure:
;;;   find-biggest-distortion
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   To retrieve the largest width/height that any distortion is allowed to be
;;; Produces:
;;;   n, a number
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   [No additional]
(define find-biggest-distortion
  (lambda (image)
    (round (* 4 (log (image-area image))))))

;;; Procedure:
;;;   find-biggest-magnifier
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   To retrieve the largest width/height that any magnifying glass is allowed
;;;   to be
;;; Produces:
;;;   n, a number
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   [No additional]
(define find-biggest-magnifier
  (lambda (image)
    (round (* 6 (log (image-area image))))))


; +---------------+--------------------------------------------------
; | Minor Helpers |
; +---------------+

(define fractal-table
  (list
   ;;;  '(name proc HOFFSET VOFFSET HSCALE VSCALE)
   (list "blade" 
         (lambda (c z) (+ c (- (real-part (expt z 3)) 
                               (* 0+i (imag-part (/ z z z))))))
         (lambda (n)
           (+ -3.0 (* 0.5 (mod n 7))))
         (lambda (n)
           (+ -3.0 (* 0.5 (mod n 7))))
         (lambda (HOFFSET)
           (- 1.0 HOFFSET))
         (lambda (VOFFSET)
           (- 1.0 VOFFSET)))
   (list "burning-ship" 
         (lambda (c z) 
           (+ (square 
               (+ (abs (real-part z)) (* 0+i (abs (imag-part z)))))
              c))
         (lambda (n)
           (+ -2.0 (* 0.5 (mod n 3))))
         (lambda (n)
           (+ -2.0 (* 0.25 (mod n 5))))
         (lambda (HOFFSET)
           (- 1.0 HOFFSET))
         (lambda (VOFFSET)
           (- 1.0 VOFFSET)))
   (list "mandelbrot" 
         (lambda (c z) 
           (+ (* z z) c))
         (lambda (n)
           (+ -2.0 (* 0.5 (mod n 3))))
         (lambda (n)
           (+ -1.0 (* 0.25 (mod n 5))))
         (lambda (HOFFSET)
           (- 1.0 HOFFSET))
         (lambda (VOFFSET)
           (- 1.0 VOFFSET)))
   (list "pythagoras-tree")
   (list "julia" 
         (lambda (c z) 
           (- (* z z) 1)))
   (list "attractor")
   (list "eagle")
   (list "void"
         (lambda (c z)
           (- (expt z (real-part c)) 1))
         (lambda (n)
           (+ -4.0 (* 0.5 (mod n 9))))
         (lambda (n)
           (+ -4.0 (* 0.5 (mod n 9))))
         (lambda (HOFFSET)
           (- 1.0 HOFFSET))
         (lambda (VOFFSET)
           (- 1.0 VOFFSET)))
   (list "sphere"
         (lambda (c z) 
           (* z z))
         (lambda (n)
           (+ -3.0 (* 0.25 (mod n 7))))
         (lambda (n)
           (+ -3.0 (* 0.25 (mod n 7))))
         (lambda (HOFFSET)
           (- 1.0 HOFFSET))
         (lambda (VOFFSET)
           (- 1.0 VOFFSET)))
   (list "newton"
         (lambda (c z)
           (- (expt z 3) 1))
         (lambda (n)
           (+ -2 (* 0.25 (mod n 3))))
         (lambda (n)
           (+ -2 (* 0.25 (mod n 3))))
         (lambda (HOFFSET)
           (- 2.0 HOFFSET))
         (lambda (VOFFSET)
           (- 2.0 VOFFSET)))))

(define lookup-attributes
  (lambda (string table)
    (let ([assoc-result (assoc string table)])
      (if (equal? assoc-result #f)
          (error "fractal not valid.")
          assoc-result))))

(define assoc
  (lambda (key alist)
    (cond
      [(null? alist) 
       #f]
      [(equal? key (car (car alist))) 
       (car alist)]
      [else 
       (assoc key (cdr alist))])))

(define bound
  (lambda (lower upper n)
    (min (max lower n) upper)))

(define image-area
  (lambda (image)
    (* (image-width image) (image-height image))))

(define determine-aoe
  (lambda (n)
    (let ([mod-aoe (modulo (* n 7) 30)])
      (if (> 10 mod-aoe)
          (+ 10 mod-aoe)
          mod-aoe))))

(define determine-blur
  (lambda (n)
    (let ([mod-blur (modulo (* n 13) 100)])
      (if (> 5 mod-blur)
          (+ 5 mod-blur)
          mod-blur))))

;;; Procedure:
;;;   unit-coord-x
;;; Parameters:
;;;   x, an integer
;;;   width, an integer
;;; Purpose:
;;;   Convert x to the range 0..1.
;;; Produces:
;;;   ux, a real number
;;; Preconditions:
;;;   0 <= x < width
;;; Postconditions:
;;;   0 <= ux < 1
;;;   (* ux width) is approximately x.
(define unit-coord-x
  (lambda (x width)
    (/ x width)))

;;; Procedure:
;;;   unit-coord-y
;;; Parameters:
;;;   y, an integer
;;;   height, an integer
;;; Purpose:
;;;   Convert y to the range 0..1.
;;; Produces:
;;;   uy, a real number
;;; Preconditions:
;;;   0 <= y < height
;;; Postconditions:
;;;   0 <= uy < 1
;;;   (* uy height) is approximately y.
(define unit-coord-y
  (lambda (y height)
    (/ y height)))

;;; Procedure:
;;;   complex-distance-squared
;;; Parameters:
;;;   complex1, a complex number
;;;   complex2, a complex number
;;; Purpose:
;;;   Computes the square of the distance from the point represented
;;;   by complex1 to the point represented by complex2
;;; Produces:
;;;   distance-squared, a real number
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   distance-squared is the distance using the standard formula.
;;; Ponderings:
;;;   We compute the squared distance, rather than the distance, because
;;;   computing distance normally requires computing a square root, and
;;;   that's likely to be expensive.
(define complex-distance-squared
  (lambda (complex1 complex2)
    (+ (square (- (real-part complex1) (real-part complex2)))
       (square (- (imag-part complex1) (imag-part complex2))))))

; +----------+-------------------------------------------------------
; | Textures |
; +----------+

;;; Procedure:
;;;   hsv2irgb
;;; Parameters:
;;;   hue, a real number
;;;   saturation, a real number
;;;   value, a real number
;;; Purpose:
;;;   Build an integer-encoded RGB color from a hue, saturation, 
;;;   and value
;;; Produces:
;;;   irgb, an integer-encoded RGB color
;;; Preconditions:
;;;   0 <= saturation <= 1
;;;   0 <= value <= 1
;;; Postconditions:
;;;   (irgb->hue irgb) is close to hue
;;;   (irgb->saturation irgb) is close to saturation
;;;   (irgb->value irgb) is close to value
;;; Philosophy:
;;;   The built-in hsv->irgb is somewhat broken and I'm too lazy
;;;   to fix it an propagate the changes everywhere.  This is a
;;;   temporary fix.
;;; Props:
;;;   Based on code developed by Janet Davis's research students.
;;;   Their names have been lost.
(define hsv2irgb
  (lambda (hue saturation value)
    (let* ([hi (mod (floor (/ hue 60)) 6)]
           [v value]
           [f (- (/ hue 60) hi)]
           [p (* value (- 1 saturation))]
           [q (* value (- 1 (* f saturation)))]
           [t (* value (- 1 (* saturation (- 1 f))))])
      (cond
        [(= hi 0) (irgb (* 255 v) (* 255 t) (* 255 p))]
        [(= hi 1) (irgb (* 255 q) (* 255 v) (* 255 p))]
        [(= hi 2) (irgb (* 255 p) (* 255 v) (* 255 t))]
        [(= hi 3) (irgb (* 255 p) (* 255 q) (* 255 v))]
        [(= hi 4) (irgb (* 255 t) (* 255 p) (* 255 v))]
        [(= hi 5) (irgb (* 255 v) (* 255 p) (* 255 q))]
        [else (irgb 0 0 0)]))))


;;; Procedure:
;;;   indexed-color
;;; Parameters:
;;;   i, an integer
;;; Purpose:
;;;   Produce a color based on an index
;;; Produces:
;;;   irgb, an integer-encoded RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If j != i and (abs (- j i)) < max-recursions, then
;;;     (indexed-color i) and (index-color j) are likely to
;;;     be different.
(define index-color
  (lambda (i)
    (hsv2irgb ;;; Change first parameter to hsv2irgb to change the color scheme of the fractals.
     (* 180 (+ 1 (* 1.5 (sin i))))
     (+ .5 (* .1 (mod i 6)))
     (+ .5 (* .05 (mod i 11))))))

; +-----------+------------------------------------------------------
; | Constants |
; +-----------+

;;; Constant:
;;;   max-recursions
;;; Type:
;;;   Integer
;;; Description:
;;;   The maximum number of iterations of the function we do before
;;;   giving up.
(define max-recursions 50)

(define initial-aoe-vector
  (list (list 0) (list 0) (list 3) (list 7)))

(define initial-magnify-vector
  (list (list 0) (list 0) (list 20)))

(define chaotic-matrix-3x3
  (list (list 1.1 1.7 21)
        (list 5 6.3 7.81)
        (list 2 4.5 9.1)))

(define chaotic-matrix-4x4-1
  (list (list 1 0.5 0.2 0.1) 
        (list 0.3 0.4 0.2 2) 
        (list 0 2 1 0) 
        (list 0 0.9 0 7)))

(define chaotic-matrix-4x4-2
  (list (list 5 2 2.5 11) 
        (list 1 2 5 0.5) 
        (list 0 1 4 0.002) 
        (list 1 3 3 7)))


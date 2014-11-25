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
           (master-series )]
          )))

(define master-series
  (lambda (n width height)
    0))

; An version of image-series that generates various portions of the
; mandelbrot set.  Not documented with the six P's because that's
; part of the project, and this is sample code for the project.
(define mandelbrot-image-series
  (lambda (n width height)
    (let* (; Our default color.  Used when we hit max-recursions.
           [DEFAULT (irgb 0 0 0)]
           ; The horizontal offset of the center from top-left.
           ; Use -2.0 for normal.  Might be based on n.
           [HOFFSET (+ -2.0 (* 0.5 (mod n 3)))]
           ; The vertical offset of the center from top-left.
           ; Use -1.0 for normal.  Might be based on n.
           [VOFFSET (+ -1.0 (* 0.25 (mod n 5)))]
           ; The horizontal scale.  Might be based on n
           [HSCALE (- 1.0 HOFFSET)]
           ; The Vertical scale.  Might be based on n.
           [VSCALE (- 1.0 VOFFSET)])
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
                  [s (steps-to-escape c)]
                  [color (if (= s -1)
                             DEFAULT
                             (index-color (+ n s)))])
             color))
         width height)))))

(define darp
  car)

(define derp
  cdr)

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

(define transpose
  (lambda (matrix)
    (if (or (null? matrix) (null? (car matrix)))
        null
        (cons (map car matrix)
              (transpose (map cdr matrix))))))

(define normalize
  (lambda (col)
    (let ([magnitude (sqrt (apply + (map square col)))])
      (map (r-s / magnitude) col))))

(define normalization
  (lambda (matrix)
    ((o transpose (l-s map normalize) transpose) matrix)))

(define eigenvector-composition
  (lambda (matrix1 matrix2)
    (matrix-multiplication 
     (matrix-multiplication matrix1 matrix2) (transpose matrix1))))


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
            (cons (chaotic-chain chaotic-matrix-4x4-2 initial-magnify-vector power) 
                  (kernel (+ power 1))))))))

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
  (lambda (complex-number)
    (let kernel ([z complex-number]
                 [steps 0])
      (cond
        [(>= (complex-distance-squared z 0) 4)
         steps]
        [(>= steps max-recursions)
         -1]
        [else
         (kernel (+ (real-part (* z z z)) (* 0+i (imag-part (/ z z z)))) (+ steps 1))]))))

; +-------------------+----------------------------------------------
; | Effect Procedures |
; +-------------------+

(define add-scaled-ellipse!
  (lambda (image base-layer left top width height aoe stroke?)
    (copy-and-add-layer! image base-layer)
    (let ([temp-layer (get-top-layer image)]
          [bound-aoe (min (min (- (image-width image)
                                  (+ left width))
                               (- (image-height image)
                                  (+ top height)))
                          aoe)])
      
      (image-select-ellipse! image REPLACE 
                             (- left bound-aoe)
                             (- top bound-aoe)
                             (+ (* 2 bound-aoe) width)
                             (+ (* 2 bound-aoe) height))
      (when stroke? 
        (repeat 20 context-set-brush! "2. Hardness 100" 9)
        (image-stroke-selection! image))
      
      (scale-selection-into-new-layer!
       temp-layer left top (+ left width) (+ top height))
      (gimp-image-remove-layer image temp-layer)
      (merge-floating-layer image (get-top-layer image) 1))))

(define scale-selection-into-new-layer!
  (lambda (temp-layer top-left-x top-left-y bot-right-x bot-right-y)
    (gimp-floating-sel-to-layer
     (car (gimp-item-transform-scale 
           temp-layer 
           top-left-x 
           top-left-y                    
           bot-right-x 
           bot-right-y)))))

(define merge-floating-layer
  (lambda (image layer-merge merge-type)
    (gimp-image-merge-down image layer-merge merge-type)))

(define blur-image!
  (lambda (image layer repititions)
    (repeat repititions plug-in-blur 1 image layer)))

(define copy-and-add-layer!
  (lambda (image layer)
    (gimp-image-add-layer 
     image
     (car (gimp-layer-copy layer 1))
     0)))

(define get-top-layer
  (lambda (image)
    (caadr (gimp-image-get-layers image))))


(define distort!
  (lambda (image distort-lst aoe blur-degree)
    (render-blobs! image distort-lst
                   3
                   (round (find-biggest-distortion image))
                   aoe blur-degree)
    (blur-image! image (get-top-layer image) blur-degree)))

(define magnifying-glass!
  (lambda (image left top diameter factor)
    (add-scaled-ellipse! 
     image (get-top-layer image) left top diameter diameter (* -1 factor) #t)
    (context-update-displays!)))


(define rain-me!
  (lambda (image raindrop-lst aoe blur-degree)
    (render-blobs! image raindrop-lst
                   3
                   (round (find-biggest-raindrop image))
                   aoe blur-degree)))

(define render-blobs!
  (lambda (image lst min-dimension max-dimension aoe blur-degree)
    (copy-and-add-layer! image (get-top-layer image))
    (let* ([layers (cadr (gimp-image-get-layers image))]
           [base (cadr layers)]
           [blurred (car layers)])
      (blur-image! image blurred blur-degree)
      (let kernel ([remaining lst])
        (display (length remaining))
        (if (null? remaining)
            (context-update-displays!)
            (let* ([raw-blob (car remaining)]
                   [blob (map (o round car) raw-blob)]
                   [width (+ min-dimension (modulo (list-ref blob 2) max-dimension))]
                   [height (+ min-dimension (modulo (list-ref blob 3) max-dimension))])
              (display  (modulo (list-ref blob 0) (- (image-width image) width)))
              (add-scaled-ellipse!
               image base
               (modulo (list-ref blob 0) (- (image-width image) width))
               (modulo (list-ref blob 1) (- (image-height image) height))
               width height aoe #f)
              (kernel (cdr remaining))))))))


(define find-biggest-raindrop
  (lambda (image)
    (- (* 4.5 (log (image-area image))) 18.5)))

(define find-biggest-distortion
  (lambda (image)
    (+ (* 0.0000895644 (image-area image)) 11.3735)))

; +---------------+--------------------------------------------------
; | Minor Helpers |
; +---------------+

(define fractal-table
  (list
   (list "eagle-fractal" (lambda (z) 1))
   (list "burning-ship")
   (list "mandelbrot")
   (list "pythagoras-tree")
   (list "julia")
   (list "attractor")
   (list "")
   (list "")
   (list "")
   (list "")))

(define num-of-inputs
  (lambda (image)
    (round (- (* 87.7275 (log (image-area image))) 669))))

(define bound
  (lambda (lower upper n)
    (min (max lower n) upper)))

(define image-area
  (lambda (image)
    (* (image-width image) (image-height image))))

(define determine-aoe
  (lambda (n)
    (let ([mod-aoe (modulo (* n 7) 100)])
      (if (> 10 mod-aoe)
          (+ mod-aoe)
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
    (hsv2irgb (* i (/ 360.0 ))
              (+ .5 (* .1 (mod i 6)))
              (+ .5 (* .05 (mod i 11))))))

(define taxicab-distance
  (lambda (col1 row1 col2 row2)
    (+ (* 1 (abs (- col2 col1))) (abs (- row2 row1)))))

(define taxicab-distance1
  (lambda (col1 row1 col2 row2)
    (+ (* 1 (abs (- col2 (/ (+ row1 row2) 2)))) 
       (abs (- row2 (/ (+ col1 col2) 2))))))

(define taxicab-distance2
  (lambda (col1 row1 col2 row2)
    (+ (* 1 (abs (- col2 (/ (+ row1 row2) 2)))) 
       (abs (- row2 (/ (- col1 col2) 2))))))

(define sin-distance1
  (lambda (col1 row1 col2 row2)
    (+ 100 (* 1 (tan (- col2 (/ (+ row1 row2) 2)))) 
       (* 1 (tan (- row2 (/ (+ col1 col2) 2)))))))

(define euclidean-distance
  (lambda (col1 row1 col2 row2)
    (sqrt (+ (square (- col2 col1)) (square (- row2 row1))))))

(define deterministic-distance
  (lambda (col1 row1 col2 row2)
    (abs (- (* col1 row2) (* col2 row1)))))

;(image-show (image-compute
;             (lambda (col row)
;               (if (and (<= (sin-distance1 200 200 col row) 100)
;             ;           (<= (taxicab-distance2 400 400 col row) 100)
;                        )
;                   (irgb (* 1/2 col) 0 (* 1/2 row))
;                   (irgb 0 0 0)))
;             400 400))


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
  (list (list 1 1 0)
        (list 2 5 4)
        (list 3 1/2 5)))

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


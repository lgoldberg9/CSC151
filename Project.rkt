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

; +---------+--------------------------------------------------------
; | Helpers |
; +---------+

(define fractal-table
  (list
   (list "eagle-fractal")
   (list "burning-ship")
   (list "mandelbrot")
   (list "pythagoras-tree")
   (list "julia")
   (list "attractor")
   (list "")
   (list "")
   (list "")
   (list "")))

; +-----------+------------------------------------------------------
; | Constants |
; +-----------+

(define darp
  car)

(define derp
  cdr)

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

(define m-add
  (lambda (v1 v2)
    (cond
      ;[()]
      ;[]
      ;[]
      [else 
       (let kernel ([lst1 v1][lst2 v2])
         (if (and (null? lst1) (null? lst2))
             null
             (cons (+ (car lst1) (car lst2)) 
                   (kernel (cdr lst1) (cdr lst2)))))]
      )))

(define m-scale
  (lambda (scalar v)
    (cond
      [(not (number? scalar))
       (error "m-scale: expects number for scalar, given" scalar)]
      [(not (list? v))
       (error "m-scale: expects list for v, given" v)]
      [else
       (let kernel ([remaining v])
         (if (null? remaining)
             null
             (cons (* (car remaining) scalar) (kernel (cdr remaining)))))]
      )))

(define matrix-dim-col
  (lambda (matrix)
    (let kernel ([sum-so-far 0][remaining-matrix (car matrix)])
      (if (null? remaining-matrix)
          sum-so-far
          (kernel (+ sum-so-far 1) (cdr remaining-matrix))))))

(define matrix-dim-row
  (lambda (matrix)
    (let kernel ([sum-so-far 0][remaining-matrix matrix])
      (if (null? remaining-matrix)
          sum-so-far
          (kernel (+ sum-so-far 1) (cdr remaining-matrix))
          ))))

(define markov-chain
  (lambda (matrix vec power)
    (let kernel ([markov-vec vec][counter 0])
      (if (= counter power)
          markov-vec
          (kernel (vector-composition matrix markov-vec) (+ 1 counter))))))

(define functional-multiplication
  (lambda (vec1 vec2)
    (apply + (map * vec1 vec2))))

(define vector-composition
  (lambda (matrix vec)
    (let ([dim (matrix-dim-row matrix)])
      (let kernel ([counter 0][matrix-final matrix])
        (if (= counter dim)
            null
            (cons (functional-multiplication (car matrix-final) vec)
                  (kernel 
                   (+ 1 counter)
                   (cdr matrix-final)
                   )))))))

(define matrix-multiplication ;;; Cite Stone.
  (lambda (left right)
    (let ([left-dim-row (length left)]
          [right-dim-row (length right)]
          [left-dim-col (length (car left))]
          [right-dim-col (length (car right))])
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

(define circle
  (lambda (image x-center y-center radius)
    (let ([diameter (* 2 radius)])
      (image-select-ellipse! image REPLACE 
                             (- x-center radius) 
                             (- y-center radius) 
                             diameter diameter))))

(define rad->degrees
  (lambda (angle) ;;; Converts an angle from radians to degrees.
    (* angle (/ 180 pi))))

(define degrees->rad
  (lambda (angle) ;;; Converts an angle from degrees to radians.
    (* angle (/ pi 180))))

(define polar-coords
  (lambda (x-center y-center col row)
    (list (euclidean-distance x-center y-center col row)
          (cond
            [(= row y-center) ;;; Point on x-axis
             0]
            [(= col x-center) ;;; Point on y-axis
             (/ pi 2)]
            [else
             (let ([arctan (atan (/ (- row y-center) (- col x-center)))])
               (cond 
                 [(and (> row y-center) (> x-center col)) ;;; Second Quadrant
                  (+ pi arctan)]
                 [(and (> y-center row) (> col x-center)) ;;; Fourth Quadrant
                  (+ (* 2 pi) arctan)]
                 [(and (> y-center row) (> x-center col)) ;;; Third Quadrant
                  (+ pi arctan)]
                 [else                                    ;;; First Quandrant
                  arctan]
                 ))]
            ) 
          x-center y-center)))

(define cartesian-coords
  (lambda (coords x-center y-center)
    (list (round (+ x-center (* (car coords) (cos (cadr coords))))) 
          ;;; x-coordinate needs to be restored to its proper position relative 
          ;;; to the x-center of the circle.
          (round (+ y-center (* (car coords) (sin (cadr coords)))))
          ;;; Likewise for y-coordinate.
          )))

;(define reflective-circle
;  (lambda (image x-center y-center aoe radius)
;    (let* ([left (- x-center aoe)]
;           [top (- y-center aoe)]
;           [width-height (* 2 radius)]
;           [new-image (image-new width-height width-height)])
;      (circle new-image x-center y-center radius)
;      (image-recompute!
;       new-image
;       (lambda (col row)
;         ;         (let* ([polars (polar-coords x-center y-center col row)]
;         ;                [scaled-radius (/ (* radius (car polars)) aoe)]
;         ;                [cartesian 
;         ;                 (cartesian-coords (list scaled-radius (cadr polars))
;         ;                                   x-center y-center)])
;         ;           (if (<= (sin-distance1 x-center y-center col1 row) aoe)
;         (image-get-pixel image col row)
;         ;               (irgb 0 0 0)))
;         )))))

(define bound
  (lambda (lower upper n)
    (min (max lower n) upper)))
    

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
                             (bound 0 (image-width image) (- left bound-aoe))
                             (bound 0 (image-height image) (- top bound-aoe))
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

(define blur-image
  (lambda (image layer repititions)
    (repeat repititions plug-in-blur 1 image layer)))

(define distort!
  (lambda (image distort-lst aoe blur-degree)
    (render-blobs! image distort-lst
                   (round (find-biggest-distortion image))
                   (round (find-biggest-distortion image))
                   aoe blur-degree)
    (blur-image image (get-top-layer image) blur-degree)))
     
    

(define magnifying-glass!
  (lambda (image left top diameter factor)
    (add-scaled-ellipse! 
     image (get-top-layer image) left top diameter diameter (* -1 factor) #t)
    (context-update-displays!)))
    
(define render-blobs!
  (lambda (image lst min-dimension max-dimension aoe blur-degree)
    (copy-and-add-layer! image (get-top-layer image))
    (let* ([layers (cadr (gimp-image-get-layers image))]
           [base (cadr layers)]
           [blurred (car layers)])
      (blur-image image blurred blur-degree)
      (let kernel ([remaining lst])
        (if (null? remaining)
            (context-update-displays!)
            (let ([blob (car remaining)])
              (add-scaled-ellipse!
               image base
               (modulo (list-ref blob 0) (image-width image))
               (modulo (list-ref blob 1) (image-height image))
               (+ min-dimension (modulo (list-ref blob 2) max-dimension))
               (+ min-dimension (modulo (list-ref blob 2) max-dimension))
               aoe
               #f)
              (kernel (cdr remaining))))))))
    
; raindrop-list: (list (list x-center y-center added-width added-height))
(define rain-me!
  (lambda (image raindrop-lst aoe blur-degree)
    (render-blobs! image raindrop-lst
                   (round (find-biggest-raindrop image))
                   (round (find-biggest-raindrop image))
                   aoe blur-degree)))

(define find-biggest-raindrop
  (lambda (image)
    (- (* 4.5 (log (image-area image))) 18.5)))

(define find-biggest-distortion
  (lambda (image)
    (+ (* 0.0000895644 (image-area image)) 11.3735)))
    
    
    
              
;(define texturize
;  (lambda (image)


(define copy-and-add-layer!
  (lambda (image layer)
    (gimp-image-add-layer 
     image
     (car (gimp-layer-copy layer 1))
     0)))

(define get-top-layer
  (lambda (image)
    (caadr (gimp-image-get-layers image))))

;(define rain-onto-layer
;  (lambda (n image layer min-width min-height delta-width delta-height aoe)
;   (let kernel ([countdown n])
;      (if (= 0 n)
;          (let ()
;            (image-select-nothing!)
;            (context-update-displays!))
;          (let ()
;            (reflective-ellipse-from-layer
;             image
;             layer
;             (random (image-width image))
;             (random (image-height image))
;             (+ min-width (random delta-width))
;             (+ min-height (random delta-height))
;             aoe)
;            (kernel (- countdown 1))
;            )))))

;(define moving-items
;  (lambda (image-to selection)
;    (gimp-edit-cut selection)
;    (gimp-edit-paste )))


(define pugbro (image-load "/home/fisherhe/Desktop/sadpug.jpg"))
(image-show pugbro)
(magnifying-glass! pugbro 530 400 200 40)
;(rain-me! pugbro 5 5 15 15 50 20 100)
 
;(reflective-ellipse-from-layer kitty 200 200 20 30 100)

;;; Constant:
;;;   MAX_ITERATIONS
;;; Type:
;;;   Integer
;;; Description:
;;;   The maximum number of iterations of the function we do before
;;;   giving up.
(define MAX_ITERATIONS 50)

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
;;;   If j != i and (abs (- j i)) < MAX_ITERATIONS, then
;;;     (indexed-color i) and (index-color j) are likely to
;;;     be different.
(define index-color
  (lambda (i)
    (hsv2irgb (* i (/ 360.0 MAX_ITERATIONS))
              (+ .5 (* .1 (mod i 6)))
              (+ .5 (* .05 (mod i 11))))))

;;; Procedure:
;;;   complex-distance-squared
;;; Parameters:
;;;   c1, a complex number
;;;   c2, a complex number
;;; Purpose:
;;;   Computes the square of the distance from the point represented
;;;   by c1 to the point represented by c2
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
  (lambda (c1 c2)
    (+ (square (- (real-part c1) (real-part c2)))
       (square (- (imag-part c1) (imag-part c2))))))

;;; Procedure:
;;;   steps-to-escape
;;; Parameters:
;;;   c, a complex number.
;;; Purpose:
;;;   Counts the number of times we can repeatedly compute z[i+1] =
;;;   z[i]*z[i]+c before hitting MAX_ITERATIONS or getting a number 
;;;   that is more than 2 away from the complex origin.  If we hit
;;;   MAX_ITERATIONS, we return a special value (-1).
;;; Produces:
;;;   s, an integer
;;; Preconditions:
;;;   Let f(z) be z*z+c.  (We use this in the postconditions.)
;;; Postconditions:
;;;   If s >= 0, then c "escapes" after exactly s iterations.  That is,
;;;     distance((f^s)(c), 0) >= 2 and for all i, 0 <= i < s, 
;;;     distance((f^i)(c), 0) < 2.
;;;   If s = -1, then c does not "escape" within MAX_ITERATIONS. That is,
;;;     for all i, 0 <= i <= MAX_ITERATIONS, distance((f^i)(c), 0) < 2.
(define steps-to-escape
  (lambda (c)
    (let kernel ([z c]
                 [s 0])
      (cond
        [(>= (complex-distance-squared z 0) 4)
         s]
        [(>= s MAX_ITERATIONS)
         -1]
        [else
         (kernel (+ (real-part (* z z z)) (* 0+i (imag-part (/ z z z))) c) (+ s 1))]))))

; An version of image-series that generates various portions of the
; mandelbrot set.  Not documented with the six P's because that's
; part of the project, and this is sample code for the project.
(define mandelbrot-image-series
  (lambda (n width height)
    (let* (; Our default color.  Used when we hit MAX_ITERATIONS.
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



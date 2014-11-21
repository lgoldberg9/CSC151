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

(define image-series
  (lambda (n width height)
    "bullshit"))

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

(define m-multiply
  (lambda (matrix1 matrix2)
    (let ([dim-row (matrix-dim-row matrix1)])
      (let row-kernel ([counter 0]
                       [mat1 matrix1]
                       [resultant null])
        (if (= counter dim-row)
            (reverse resultant)
            (row-kernel (+ counter 1) (cdr mat1) 
                        (cons (functional-kernel mat1 matrix2) resultant)))
        ))))
(define functional-kernel 
  (lambda (matrix1 matrix2)
    (let kernel ([m1 (car matrix1)][m2 (car matrix2)])
      (if (and (null? m1) (null? m2))
          0
          (+ (* (car m1) (car m2))
             (kernel (cdr m1) (cdr m2)))))))

(define m-power
  (lambda (matrix power)
    (let kernel ([counter 1])
      (if (= counter power)
          (m-multiply matrix matrix)
          (m-multiply matrix (kernel (+ counter 1)))))))

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

(define reflective-circle
  (lambda (image x-center y-center aoe radius)
    (let* ([left (- x-center aoe)]
           [top (- y-center aoe)]
           [width-height (* 2 radius)]
           [new-image (image-new width-height width-height)])
      (circle new-image x-center y-center radius)
      (image-recompute!
       new-image
       (lambda (col row)
         ;         (let* ([polars (polar-coords x-center y-center col row)]
         ;                [scaled-radius (/ (* radius (car polars)) aoe)]
         ;                [cartesian 
         ;                 (cartesian-coords (list scaled-radius (cadr polars))
         ;                                   x-center y-center)])
         ;           (if (<= (sin-distance1 x-center y-center col1 row) aoe)
         (image-get-pixel image col row)
         ;               (irgb 0 0 0)))
         )))))

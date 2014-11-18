#lang racket
(require gigls/unsafe)

;;; File:
;;;   Final_Project.rkt
;;; Authors:
;;;   Logan Goldberg
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

(define circle
  (lambda (image x-center y-center radius)
    (let ([diameter (* 2 radius)])
      (image-select-ellipse! image REPLACE 
                             (- x-center radius) 
                             (- y-center radius) 
                             diameter diameter))))

(define rad->degrees
  (lambda (angle)
    (* angle (/ 180 pi))))

(define polar-coords
  (lambda (x-center y-center col row)
    (list (euclidean-distance x-center y-center col row)
          (atan (/ (- row y-center) (- col x-center))))))

(define cartesian-coords
  (lambda (coords)
    (list (* (car coords) (cos (cadr coords)))
          (* (car coords) (sin (cadr coords))))))

(define reflective-circle
  (lambda (image x-center y-center aoe radius)
    (let ([left (- x-center aoe)]
          [top (- y-center aoe)]
          [width-height (* 2 aoe)])
      (circle image x-center y-center aoe)
      (image-recompute!
       (lambda (col row)
         (let* ([polars (polar-coords x-center y-center col row)]
                [scaled-radius (/ (* radius (car polars)) aoe)]
                [cartesian 
                 (cartesian-coords (list scaled-radius (cadr polars)))])
         (image-set-pixel! (image-new 1000 1000) (car cartesian) (cadr cartesian)
          (image-get-pixel image col row))))))))
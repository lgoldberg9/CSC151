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
    (cond [(= n 666)
           (image-show (image-load "/home/goldberg/Pictures/samcollage.png"))]
;          [
;           ]
;          [
;           ]
;          [
;           ]
;          [
;           ]
;          [
;           ]
;          [
;           ]
;          [
;           ]
;          [
;           ]
;          [
;           ]
;          [else 
;           ]
          )))

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

(define reflective-ellipse-from-layer
  (lambda (image base blurred left-x left-y width height aoe)
    (copy-and-add-layer! image base)
    (let ([temp-layer (get-top-layer image)])
      (image-select-ellipse! image REPLACE 
                             (- left-x aoe) 
                             (- left-y aoe)
                             (+ (* 2 aoe) width)
                             (+ (* 2 aoe) height))
      
      (gimp-floating-sel-to-layer
       (car (gimp-item-transform-scale temp-layer left-x left-y 
                                       (+ left-x width) (+ left-y height))))
      (gimp-image-remove-layer image temp-layer)
      (merge-floating-layer image (get-top-layer image) 1)
      ;(display temp-layer) (newline) (display (gimp-image-get-layers image))
      )))

(define merge-floating-layer
  (lambda (image layer-merge merge-type)
    (gimp-image-merge-down image layer-merge merge-type)))

(define blur-image
  (lambda (image layer repititions)
    (repeat repititions plug-in-blur 1 image layer)))

(define rain-me!
  (lambda (image min-width min-height delta-width delta-height blur-degree aoe k)
    (copy-and-add-layer! image (caadr (gimp-image-get-layers image)))
    (let* ([layers (cadr (gimp-image-get-layers image))]
           [base (cadr layers)]
           [blurred (car layers)])
      (blur-image image blurred blur-degree)
      (let kernel ([counter 0])
        (if (= counter k)
            (context-update-displays!)
            (let ()
              (reflective-ellipse-from-layer 
               image base blurred 
               (random (image-width image))
               (random (image-height image))
               (+ min-width (random delta-width)) 
               (+ min-height (random delta-height))  
               aoe)
              (kernel (+ counter 1))))))))

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


(define image (image-load "/home/goldberg/Downloads/cameron-highlands.jpg"))
(image-show image)
;(reflective-ellipse-from-layer kitty 200 200 20 30 100)

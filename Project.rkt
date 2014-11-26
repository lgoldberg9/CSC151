#lang racket
(require gigls/unsafe)

;;; File:
;;;   Final_Project.rkt
;;; Authors:
;;;   Logan Goldberg, Henry Fisher
;;; Summary:
;;;   

; +-------+----------------------------------------------------------
; | Notes |
; +-------+
;;; Hereinafter, we shall refer to what is known as a column-vector.
;;; A column-vector is a list of lists where each sub-list has one element.
;;; For example, (list (list 1) (list 4) (list 2)) is a three-dimensional
;;; column-vector. Each number in our column-vector will be a real number 
;;; by default.
;
;
;;; Props:
;;; I would like to thank Professor Mileti for suggesting how to 
;;; go about making our matrices chaotic, Professor Holcomb for
;;; suggesting the types of magnification we used, Professor Weiman
;;; for suggesting how to alter our color schcme in our fractals,
;;; Professor Stone for helping us work out some of the quirks in
;;; our procedures, and Professor Rebelsky for all the nifty procedures
;;; we used in our project.

; +--------------------+---------------------------------------------
; | Primary Procedures |
; +--------------------+

(define image-series
  (lambda (n width height)
    (cond [(= n 666)
           (image-show (image-load "/home/goldberg/public_html/images/samcollage.png"))]
          [(= n 420)
           (image-show (image-load "/home/goldberg/public_html/images/stonecollage.png"))]
          [(= n 42)
           (image-show (image-load "/home/goldberg/public_html/images/weinmancollage.png"))]
          [(= n 888)
           (image-show (image-load "/home/goldberg/public_html/images/daviscollage.png"))]
          [(= n 0)
           (image-show (image-load "/home/goldberg/public_html/images/walkercollage.png"))]
          [else
           (master-series n width height)]
          )))

(define master-series
  (lambda (n width height)
    (let ([fractal (fractal-image-series n width height)])
      (cond 
        [(>= 10000 (image-area fractal))
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
    (let* ([FRACTAL-CHOICE (car 
                            (list-ref fractal-table 
                                      (modulo (round (/ n 4)) (length fractal-table))))]
           [ASSOC-FRACTAL (lookup-attributes FRACTAL-CHOICE fractal-table)]
           [DEFAULT (irgb 0 0 0)]
           [HOFFSET ((list-ref ASSOC-FRACTAL 2) n)]
           [VOFFSET ((list-ref ASSOC-FRACTAL 3) n)]
           [HSCALE ((list-ref ASSOC-FRACTAL 4) HOFFSET)]
           [VSCALE ((list-ref ASSOC-FRACTAL 5) VOFFSET)])
      (let ([complicate (lambda (x y)
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

;;; Procedure:
;;;   dim-row
;;; Parameters:
;;;   matrix, a square matrix of real numbers
;;; Purpose:
;;;   To determine the number of rows in the matrix.
;;; Produces:
;;;   dimension, a positive integer.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   [No additional]

(define dim-row
  (lambda (matrix)
    (length matrix)))

;;; Procedure:
;;;   dim-col
;;; Parameters:
;;;   matrix, a square matrix of real numbers
;;; Purpose:
;;;   To determine the number of columns in the matrix.
;;; Produces:
;;;   dimension, a positive integer.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   [No additional]

(define dim-col
  (lambda (matrix)
    (let ([mcar (car matrix)])
      (if (not (list? mcar))
          (length matrix)
          (length mcar)))))

;;; Procedure:
;;;   chaotic-chain
;;; Parameters:
;;;   matrix, a square matrix of real numbers
;;;   vec, a column-vector of real numbers
;;;   power, a nonnegative integer
;;; Purpose:
;;;   Raises a 'matrix' to some 'power' and then multiplies the resulting matrix by 'vec'.
;;; Produces:
;;;   chaotic-vec, a column-vector
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (dim-row vec)=(dim-row chaotic-vec)
;;;   (* (expt matrix power) vec)=chaotic-vec

(define chaotic-chain
  (lambda (matrix vec power)
    (let kernel ([chaotic-vec vec][counter 0])
      (if (= counter power)
          chaotic-vec
          (kernel (matrix-multiplication matrix chaotic-vec) (+ 1 counter))))))

;;; Procedure:
;;;   matrix-multiplication
;;; Parameters:
;;;   left, a square matrix of real numbers
;;;   right, a square matrix of real numbers
;;; Purpose:
;;;   Finds the product of two matrices.
;;; Produces:
;;;   product, a square matrix of real numbers
;;; Preconditions:
;;;   (dim-col left)=(dim-row right)
;;; Postconditions:
;;;   (dim-row left)=(dim-row product)
;;;   (dim-col right)=(dim-col product)
;;; Props:
;;;    Mr. Stone helped us figure out the nested recursions necessary to make this procedure work.

(define matrix-multiplication
  (lambda (left right)
    (let ([left-dim-row (dim-row left)]
          [right-dim-row (dim-row right)]
          [left-dim-col (dim-col left)]
          [right-dim-col (dim-col right)])
      (if (= left-dim-col right-dim-row)
          (let left-row ([left-row-cycle 0]) ;;;This sets the row of 'left' to recurse over.
            (if (= left-dim-row left-row-cycle)
                null
                (cons 
                 (let right-col ([right-col-cycle 0]) ;;;This sets the column of 'right' to recurse over.
                   (if (= right-dim-col right-col-cycle)
                       null
                       (cons 
                        (let vec-functional ([position 0]) ;;;This carries out the multiplication between 'left-row' and 'right-col'.
                          (if (= position right-dim-row)
                              0
                              (+ (* (list-ref (list-ref left left-row-cycle) position)
                                    (list-ref (list-ref right position) right-col-cycle))
                                 (vec-functional (+ position 1)))))
                        (right-col (+ right-col-cycle 1)))))
                 (left-row (+ left-row-cycle 1)))))
          (error "Inner-dimensions do not match. Multiplication undefined.")
          ))))

;;; Procedure:
;;;   raindrop-inputs
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Creates a list of 'column-vectors' to be used to render raindrops onto image.
;;; Produces:
;;;   inputs, a list of 'column-vectors'.
;;; Preconditions:
;;;   (> (image-area image) 10000). If this is not true, then 'raindrop-inputs'
;;;   does not execute.
;;; Postconditions:
;;;   (length inputs)=num
;;;   Each element of 'inputs' should be a four-dimensional column-vector.

(define raindrop-inputs
  (lambda (image)
    (let ([num (num-of-inputs image)])
      (let kernel ([power 0])
        (if (= power num)
            null
            (cons (chaotic-chain chaotic-matrix-4x4-1 initial-aoe-vector power) 
                  (kernel (+ power 1))))))))

;;; Procedure:
;;;   raindrop-inputs
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Creates a list of 'column-vectors' to be used to render the magnifiers onto image.
;;; Produces:
;;;   inputs, a list of 'column-vectors'.
;;; Preconditions:
;;;   (> (image-area image) 10000). If this is not true, then 'magnify-inputs'
;;;   does not execute.
;;; Postconditions:
;;;   (length inputs)=num
;;;   Each element of 'inputs' should be a three-dimensional column-vector.

(define magnify-inputs
  (lambda (image)
    (let ([num (num-of-inputs image)])
      (let kernel ([power 0])
        (if (= power num)
            null
            (cons (chaotic-chain chaotic-matrix-3x3 initial-magnify-vector power) 
                  (kernel (+ power 1))))))))

;;; Procedure:
;;;   distortion-inputs
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Creates a list of 'column-vectors' to be used to render distortions onto image.
;;; Produces:
;;;   inputs, a list of 'column-vectors'.
;;; Preconditions:
;;;   (> (image-area image) 10000). If this is not true, then 'distortion-inputs'
;;;   does not execute.
;;; Postconditions:
;;;   (length inputs)=num
;;;   Each element of 'inputs' should be a four-dimensional column-vector.

(define distortion-inputs
  (lambda (image)
    (let ([num (num-of-inputs image)])
      (let kernel ([power 0])
        (if (= power num)
            null
            (cons (chaotic-chain chaotic-matrix-4x4-2 initial-aoe-vector power) 
                  (kernel (+ power 1))))))))

;;; Procedure:
;;;   num-of-inputs
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   To determine the number of inputs to be rendered onto the image 
;;;   based off of a logarithmic function.
;;; Produces:
;;;   input-num, a nonnegative integer.
;;; Preconditions:
;;;   (> (image-area image) 10000). If this is not true, then 'num-of-inputs'
;;;   does not execute.
;;; Postconditions:
;;;   [No additional]

(define num-of-inputs
  (lambda (image)
    (round (* 10 (log (image-area image))))))

;;; Procedure:
;;;   steps-to-escape
;;; Parameters:
;;;   complex-number, a complex number.
;;;   fractal, a procedure.
;;; Purpose:
;;;   Counts the number of times we can repeatedly compute fractal
;;;   before hitting max-recursions or getting a number 
;;;   that is more than 2 away from the complex origin. If we hit
;;;   max-recursions, we return a special value (-1).
;;; Produces:
;;;   steps, an integer
;;; Preconditions:
;;;   Let f(z) be fractal.  (We use this in the postconditions.)
;;;   Have fractal be passed into 'steps-to-escape' from 'fractal-image-series'
;;;   so as to pass the assoc-list from fractal-table.
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
        (repeat 5 context-set-brush! "2. Hardness 100" 3)
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
                   3
                   (round (find-biggest-distortion image))
                   aoe 0 #t)
    (blur-image image (get-top-layer image) blur-degree)))

(define magnifying-glass!
  (lambda (image magnify-lst factor)
    (let kernel ([remaining magnify-lst])
      (if (null? remaining)
          (context-update-displays!)
          (let ([glass (car remaining)]
                [max-diameter (round (find-biggest-magnifier image))])
            (add-scaled-ellipse! 
             image (get-top-layer image)
             (modulo (round (car (list-ref glass 0))) (image-width image))
             (modulo (round (car (list-ref glass 1))) (image-height image))
             (+ 11 (modulo (round (car (list-ref glass 2))) max-diameter))
             (+ 11 (modulo (round (car (list-ref glass 2))) max-diameter))
             (* -1 factor) #t)
            (kernel (cdr remaining)))))))

(define render-blobs!
  (lambda (image lst min-dimension max-dimension aoe blur-degree stroke?)
    (copy-and-add-layer! image (get-top-layer image))
    (let* ([layers (cadr (gimp-image-get-layers image))]
           [base (cadr layers)]
           [blurred (car layers)])
      (blur-image image blurred blur-degree)
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

(define rain-me!
  (lambda (image raindrop-lst aoe blur-degree)
    (render-blobs! image raindrop-lst
                   3
                   (round (find-biggest-raindrop image))
                   aoe blur-degree #f)))

(define copy-and-add-layer!
  (lambda (image layer)
    (gimp-image-add-layer 
     image
     (car (gimp-layer-copy layer 1))
     0)))

(define get-top-layer
  (lambda (image)
    (caadr (gimp-image-get-layers image))))

(define find-biggest-raindrop
  (lambda (image)
    (* 2.4 (log (image-area image)))))

(define find-biggest-distortion
  (lambda (image)
    (* 4 (log (image-area image)))))

(define find-biggest-magnifier
  (lambda (image)
    (* 6 (log (image-area image)))))


; +---------------+--------------------------------------------------
; | Minor Helpers |
; +---------------+

;;; Procedure:
;;;   fractal-table
;;; Purpose:
;;;   We will use fractal-table to store the information of our fractals.
;;;   fractal-table is an association list that 'fractal-image-series'
;;;   and 'steps-to-escape' takes as inputs in order to build the fractal image.
;;; Contents:
;;;   The structure of the association list is as follows:
;;;   (list '(name proc HOFFSET VOFFSET HSCALE VSCALE)
;;;       'name' is the name of the fractal.
;;;       'proc' is the procedure that the values of 'z' and 'c' pass through to
;;;          calculate the 'steps-to-escape'.
;;;       'HOFFSET' is the horizontal offset from the origin. It is a procedure.
;;;       'VOFFSET' is the vertical offset from the origin. It is a procedure.
;;;       'HSCALE' is the horizontal span of our image. 
;;;          It is a procedure and is dependent on the unit-coord-x.
;;;       'VSCALE' is the vertical span of our image. 
;;;          It is a procedure and is dependent on the unit-coord-y.
(define fractal-table
  (list
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
   (list "julia" 
         (lambda (c z) 
           (- (* z z) 1)))
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

;;; Procedure:
;;;   lookup-attributes
;;; Parameters:
;;;   string, a string
;;;   table, a list of fractal entries
;;; Purpose:
;;;   Looks up the fractal in the table.
;;; Produces:
;;;   assoc-result, the list with 'string'.
;;; Preconditions:
;;;   Each entry in table must be a list.
;;;   Element 0 of each entry must be a string which represents a fractal name.
;;; Postconditions:
;;;   If an entry for the name appears somewhere in the table, 'assoc-result' is
;;;     the corresponding list that contains 'string' (computed from the components).
(define lookup-attributes
  (lambda (string table)
    (let ([assoc-result (assoc string table)])
      (if (equal? assoc-result #f)
          (error "fractal not valid.")
          assoc-result))))

;;; Procedure:
;;;   assoc
;;; Parameters:
;;;   key, a Scheme value
;;;   alist, an association list
;;; Purpose:
;;;   Find an entry with key key in alist.
;;; Produces:
;;;   entry, a Scheme value
;;; Preconditions:
;;;   No additional
;;; Postconditions:
;;;   If there is an index, i, such that
;;;     (equal? key (car (list-ref alist i)))
;;;   then entry is the first such entry
;;;   Otherwise, entry is false (#f)
(define assoc
  (lambda (key alist)
    (cond
      [(null? alist) 
       #f]
      [(equal? key (car (car alist))) 
       (car alist)]
      [else 
       (assoc key (cdr alist))])))

;;; Procedure:
;;;   bound
;;; Parameters:
;;;   lower, a real number
;;;   upper, a real number
;;;   n, a real number
;;; Purpose:
;;;   To bound 'n' to a certain domain given the upper-bound 'upper'
;;;   and the lower-bound 'lower'.
;;; Produces:
;;;   bounded-n, a nonnegative integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (< lower bounded-n upper)
;;;   if (= n upper), then (= bounded-n upper)
;;;   if (= n lower), then (= bounded-n lower)
(define bound
  (lambda (lower upper n)
    (min (max lower n) upper)))

;;; Procedure:
;;;   image-area
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   To calculate the area of the image.
;;; Produces:
;;;   area, a positive integer.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   [No additional]
(define image-area
  (lambda (image)
    (* (image-width image) (image-height image))))

;;; Procedure:
;;;   determine-aoe
;;; Parameters:
;;;   n, a nonnegative integer
;;; Purpose:
;;;   To calculate a given area-of-effect (aoe) for render-blobs! to use.
;;; Produces:
;;;   aoe, a nonnegative integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (> 10 aoe), then (+ 10 aoe).
(define determine-aoe
  (lambda (n)
    (let ([mod-aoe (modulo (* n 7) 30)])
      (if (> 10 mod-aoe)
          (+ 10 mod-aoe)
          mod-aoe))))

;;; Procedure:
;;;   determine-blur
;;; Parameters:
;;;   n, a nonnegative integer
;;; Purpose:
;;;   To calculate a given blur-effect for render-blobs! to use.
;;;   blur serves as a parameter to a repeat function which will 
;;;   repeat (plug-in-blur) 'blur' times.
;;; Produces:
;;;   blur, a nonnegative integer.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (> 5 blur), then (+ 5 blur).
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

;;; Constant:
;;;   initial-aoe-vector
;;; Type:
;;;   four-dimensional column-vector
;;; Description:
;;;   Is a column-vector that stores the set of initial conditions for
;;;   our inputs to render-blob!.
;;;   The first element is 'left' of the blob.
;;;   The second element is 'top' of the blob.
;;;   The third element is 'width' of the blob.
;;;   The fourth element is 'height' of the blob.
(define initial-aoe-vector
  (list (list 0) (list 0) (list 3) (list 7)))

;;; Constant:
;;;   initial-magnify-vector
;;; Type:
;;;   three-dimensional column-vector
;;; Description:
;;;   Is a column-vector that stores the set of initial conditions for
;;;   our inputs to render-blob!.
;;;   The first element is 'left' of the blob.
;;;   The second element is 'top' of the blob.
;;;   The third element is 'diameter' of the blob.

(define initial-magnify-vector
  (list (list 0) (list 0) (list 20)))

;;; Constant:
;;;   chaotic-matrix-3x3
;;; Type:
;;;   a square-matrix of real numbers
;;; Description:
;;;   Is a 3x3 square matrix whose inputs are real numbers.
;;;   Its designed to produce a chaotic outputs when multiplied
;;;   against a column vector repeatedly.
;;;   This matrix is used specifically for 'magnifying-glass!'.
(define chaotic-matrix-3x3
  (list (list 1.1 1.7 21)
        (list 5 6.3 7.81)
        (list 2 4.5 9.1)))

;;; Constant:
;;;   chaotic-matrix-4x4-1
;;; Type:
;;;   a square-matrix of real numbers
;;; Description:
;;;   Is a 4x4 square matrix whose inputs are real numbers.
;;;   Its designed to produce a chaotic outputs when multiplied
;;;   against a column vector repeatedly. This matrix multiplication
;;;   grows slowly and is intended for the raindrops.
(define chaotic-matrix-4x4-1
  (list (list 1 0.5 0.2 0.1) 
        (list 0.3 0.4 0.2 2) 
        (list 0 2 1 0) 
        (list 0 0.9 0 7)))

;;; Constant:
;;;   chaotic-matrix-4x4-2
;;; Type:
;;;   a square-matrix of real numbers
;;; Description:
;;;   Is a 4x4 square matrix whose inputs are real numbers.
;;;   Its designed to produce a chaotic outputs when multiplied
;;;   against a column vector repeatedly. This matrix multiplication
;;;   grows quickly and is intended for the distortions.
(define chaotic-matrix-4x4-2
  (list (list 5 2 2.5 11) 
        (list 1 2 5 0.5) 
        (list 0 1 4 0.002) 
        (list 1 3 3 7)))


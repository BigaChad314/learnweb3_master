#lang racket

(define (square x) x * x)

(define (real-part z) (* (magnitude z) (cos (angle z))))
(define (imag-part z) (* (magnitude z) (sin (angle z))))
(define magnitude car)
(define angle cdr)
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define make-from-mag-ang cons)

(define (add-complex z1 z2)
  (make-from-real-imag
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

(define z1 (add-complex (make-from-real-imag 1 2) (make-from-real-imag 3 4)))
(define z2 (mul-complex (make-from-mag-ang 5 1) (make-from-mag-ang 6 2)))
(real-part z1)
(imag-part z1)
(magnitude z2) 
(angle z2) 
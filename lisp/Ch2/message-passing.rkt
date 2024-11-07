#lang racket

(define (square x)
    x * x)

(define (make-from-real-imag x y)
  (lambda (op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error 'make-from-real-imag "unknown op" op)))))

(define (apply-generic op arg) (arg op))

(apply-generic 'imag-part (make-from-real-imag 3 4))
(apply-generic 'magnitude (make-from-real-imag 0 1))

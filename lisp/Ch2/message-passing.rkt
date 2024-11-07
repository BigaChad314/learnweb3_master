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

;; make-from-mag-ang
(define (make-from-mag-ang r a)
  (lambda (op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error 'make-from-mag-ang "unknown op" op)))))

(apply-generic 'magnitude (make-from-mag-ang 15 0.5))
(apply-generic 'imag-part (make-from-mag-ang 1 0))
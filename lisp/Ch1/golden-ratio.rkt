#lang racket/base

(define tolerance 0.00001)
(define (average a b)
    (/ (+ a b) 2))

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(display (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)) (newline)
(display (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)) (newline)
(display (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)) (newline)

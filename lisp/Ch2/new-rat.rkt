#lang racket

(define (make-rat n d)
    (cons n d))

(define (numer x)
    (let ((g (gcd (car x) (cdr x))))
        (/ (car x) g)))

(define (denom x)
    (let ((g (gcd (car x) (cdr x))))
        (/ (cdr x) g)))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; --- Tests ---

(print-rat (make-rat  2 -4))
(print-rat (make-rat -2  4))
(print-rat (make-rat 3  4))
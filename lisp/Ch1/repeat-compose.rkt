#lang racket

(define (square x) (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(display ((repeated square 2) 5)) (newline)
(define x 10)
(display (- 1 x)) (newline)
(display (- x 1)) (newline)
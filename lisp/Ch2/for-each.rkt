#lang racket

(define (for-each f xs)
  (unless (null? xs)
    (f (car xs))
    (for-each f (cdr xs))))

(for-each
 (lambda (x)
   (newline)
   (display x))
 (list 57 321 88))

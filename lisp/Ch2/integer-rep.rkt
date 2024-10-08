#lang racket

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car x)
;; cons로 결합된 값에서 2로 몇번 나눠 떨어지는지를 반환. 
  (define (car-iter x count)
  (if (= 0 (remainder x 2))
      (car-iter (/ x 2) (+ 1 count)) count))
  (car-iter x 0))

(define (cdr x)
;; cons로 결합된 값에서 3으로 몇번 나눠 떨어지는지를 반환. 
  (define (cdr-iter x count)
  (if (= 0 (remainder x 3))
      (cdr-iter (/ x 3) (+ 1 count)) count))
  (cdr-iter x 0))

(print (cons 3 18))       (newline)
(print (car (cons 5 18))) (newline)
(print (cdr (cons 3 18))) (newline)
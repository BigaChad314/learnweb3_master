#lang racket

(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

(define (length items)
    (if (null? items)
    0
    (+ 1 (length (cdr items)))))

;; (define (last-pair items)
;;     (list-ref items (- (length items) 1))) // 원소를 return 할때.

(define (last-pair xs)
  (if (null? (cdr xs))
      xs
      (last-pair (cdr xs))))

(last-pair (list 23 72 149 34))
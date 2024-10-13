#lang racket

;; // Unless를 활용한 for-each
(define (for-each f xs)
  (unless (null? xs)
    (f (car xs))
    (for-each f (cdr xs))))

;; // 한글 설명의 for-each
;; (define (for-each proc items)
;;   (if (null? items)
;;       #t
;;       (and (proc (car items))
;;            (for-each proc (cdr items)))))


(for-each
 (lambda (x)
   (newline)
   (display x))
 (list 57 321 88))

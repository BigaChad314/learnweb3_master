#lang racket

(define (element-of-set? x set)
  (and (not (null? set))
       (or (equal? x (car set))
           (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((null? set1) '())
        ((null? set2) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1) 
                        (cons (car set1) set2)))))

(define (overlap-adjoin x set)
    (cons x set))

(define (overlap-union set1 set2)
    (append set1 set2))

(adjoin-set 2 '(2 3))
(element-of-set? 1 '(3 2 1))
(intersection-set '(1) '(1 2))
(union-set '(3) '(1 2))
(union-set '(3 4 5 1) '(1 2))
(newline)
(overlap-adjoin 2 '(2 3))
(overlap-union '(3 4 5 1) '(1 2))
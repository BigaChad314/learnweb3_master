#lang racket

(define (scale-tree tree factor)
    (cond ((null? tree) '())
    ((not (pair? tree)) (* tree factor))
    (else (cons (scale-tree (car tree) factor)
                (scale-tree (cdr tree) factor)))))

;; Using Map
(define (scale-tree-map tree factor)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (scale-tree-map sub-tree factor)
                (* sub-tree factor)))
        tree))

(define (square x)
  (* x x))

;; (define (square-list items)
;;   (if (null? items)
;;       empty
;;       (cons (square (car items))
;;             (square-list (cdr items)))))

(define (square-tree tree)
    (cond ((null? tree) '())
        ((pair? tree) (cons (square-tree (car tree))
                        (square-tree (cdr tree))))
        (else (square tree))))

(define (square-tree-map x)
  (define (square-if-number x)
    (if (pair? x)
        (map square-if-number x)
        (square x)))
  (map square-if-number x))


(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree-map (list 10 (list 2 (list 30 4) 50) (list 6 70)))
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

;; Part 1. Direct recursion
(define (square-tree tree)
    (cond ((null? tree) '())
        ((pair? tree) (cons (square-tree (car tree))
                        (square-tree (cdr tree))))
        (else (square tree))))

;; Part 2. Map
(define (square-tree-map tree)
    (map (lambda (tree)
            (if (pair? tree)
                (square-tree-map tree)
                (square tree)))
        tree))

;; 차수 높은 tree-map procedure
(define (tree-map f tree)
    (map (lambda (tree)
        (if (pair? tree)
            (tree-map f tree)
            (f tree)))
        tree))

(define (square-tree-map2 tree) (tree-map square tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree-map (list 10 (list 2 (list 30 4) 50) (list 6 70)))
(square-tree-map2 (list 1 (list 20 (list 3 40) 5) (list 60 7)))
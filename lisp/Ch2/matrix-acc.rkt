#lang racket

;; Basis: Accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; Accumulate-n: "차례열들의 차례열"을 인자로 받아, 각 원소들을 합쳐 하나의 차례열로 만듦.
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 s)

;; For Matrix
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row-from-m)
           (map (lambda (column-from-n)
                  (dot-product row-from-m column-from-n))
                cols))
         m)))


(define m1 (list (list 1 2 3) (list 4 5 6)))
(define m2 (list (list 10 11) (list 20 21) (list 30 31)))
(define v1 (list 100 200))
(define v2 (list 33 44))

(newline)
(dot-product v1 v2)
(matrix-*-vector m2 v1)
(matrix-*-matrix m1 m2)
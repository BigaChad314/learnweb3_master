#lang racket

;; Basis: Accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


;; Map
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              '()
              sequence))

;; Append
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;; Length
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; Test
(define li-1 (list 1 2 3 4 5))
(define li-2 (list 60 70 80))

(define (square x) (* x x))
(map square li-1)
(append li-1 li-2)
(length li-1)
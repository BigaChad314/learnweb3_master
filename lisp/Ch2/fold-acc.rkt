#lang racket

;; Basis: Accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                    (cdr rest))))
    (iter initial sequence))

;; Part 1. Testing
(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))

;; Part 2. reverse
(define (reverse-right sequence)
    (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-left sequence)
    (fold-left (lambda (x y) (cons y x)) '() sequence))

(define x (list (list 1 2) (list 3 4)))
(display x)
(newline)
(reverse-right x)
(reverse-left x)
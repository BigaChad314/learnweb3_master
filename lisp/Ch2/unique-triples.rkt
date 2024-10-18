#lang racket

;; Basis: Accumulate 
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


;; unique-triples

(define (unique-triples n)
  (let ((range (enumerate-interval 1 n)))
    (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             range))
                      range))
           range)))

(define (triple-sums n s)
  (filter (lambda (t)
            (= s (+ (car t) (cadr t) (caddr t))))
          (unique-triples n)))

(triple-sums 10 4)
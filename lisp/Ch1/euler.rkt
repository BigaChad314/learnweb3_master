#lang racket/base

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= 0 i)
        result
        (iter (sub1 i) (/ (n i) (+ result (d i))))))
  (iter (sub1 k) (/ (n k) (d k))))

(define (d-euler i)
  (if (= (modulo i 3) 2)
      (* 2(/ (+ i 1) 3))
      1))

(define (display-serie f n)
  (define (rec i)
    (display (f i)) (display ", ")
    (if (= i n)
        (newline)
        (rec (add1 i))))
  (rec 1))

(display-serie d-euler 12)

;  exact result is 0.718281828459
(cont-frac-iter (lambda (i) 1.0)
                 d-euler
                 1)  (newline)
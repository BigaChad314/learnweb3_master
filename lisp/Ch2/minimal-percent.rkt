#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (lower-bound i) (upper-bound i)) 2))

(define (percent x)
    (* (/ (width x) (center x)) 100))

(define (make-center-percent c p)
    (let ((w (* c p 0.01))) ; c에 p%를 곱하여 w 값을 구함. w = c * p * 0.01
    (make-interval (- c w) (+ c w)))) ; w를 c에서 빼고 더해서 구간 생성

(define i (make-center-percent 30 1))
(define j (make-center-percent 25 3))
(define i*j (mul-interval i j))
(+ (percent i) (percent j))
(percent i*j)
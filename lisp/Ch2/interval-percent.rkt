#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c p)
    (let ((w (* c p 0.01))) ; c에 p%를 곱하여 w 값을 구함. w = c * p * 0.01
    (make-interval (- c w) (+ c w)))) ; w를 c에서 빼고 더해서 구간 생성

(define (percent x)
    (* (/ (width x) (center x)) 100))
    
(define R (make-center-percent 100 10))

(display R)
(newline)
(width R)
(center R)
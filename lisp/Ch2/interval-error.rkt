#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (let ((y1 (lower-bound y))
        (y2 (upper-bound y)))
    (if (<= y1 0 y2)
        (error 'div-interval "can't divide by an interval spanning zero" y)
        (mul-interval x (make-interval (/ y2) (/ y1))))))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (lower-bound i) (upper-bound i)) 2))

(define (percent x)
    (* (/ (width x) (center x)) 100))

(define (make-center-percent c p)
    (let ((w (* c p 0.01))) ; c에 p%를 곱하여 w 값을 구함. w = c * p * 0.01
    (make-interval (- c w) (+ c w)))) ; w를 c에서 빼고 더해서 구간 생성

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one
     (add-interval (div-interval one r1)
                   (div-interval one r2)))))

(define r1 (make-center-percent 10000 5))
(define r2 (make-center-percent 330 10))
(percent (par1 r1 r2))
(percent (par2 r1 r2))
(newline)

(define i (make-center-percent 5000 2))
(define j (make-center-percent 2500 1))
(center (div-interval i i)) ; ideally should be 1
(percent (div-interval i i)) ; ideally should be 0%
(center (div-interval i j)) ; correct
(percent (div-interval i j)) ; correct
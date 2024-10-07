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
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;; Addition/subtraction <> Multiplication/Division 의 구간 계산은 다른값을 도출한다.
(define x (make-interval 0 10))
(define y (make-interval 4 6))
(width x) 
(width y) 
(width (mul-interval x y)) 
(width (div-interval x y)) 

(newline)

(define a (make-interval -5 5))
(define b (make-interval -1 1))
(width a) 
(width b)
(width (mul-interval a b)) 
(width (div-interval a b))
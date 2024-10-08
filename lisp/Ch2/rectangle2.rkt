#lang racket

(define (perimeter-rect r)
  (* 2 (+ (width-rect r) (height-rect r))))

(define (area-rect r)
  (* (width-rect r) (height-rect r)))
  
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (distance-point p1 p2)
  (sqrt
   (+ (sqr (- (x-point p1) (x-point p2)))
      (sqr (- (y-point p1) (y-point p2))))))

; Helper
(define (dot-product p1 p2)
  (+ (* (x-point p1) (x-point p2))
     (* (y-point p1) (y-point p2))))

(define (add-vector v1 v2)
  (make-point (+ (x-point v1) (x-point v2))
              (+ (y-point v1) (y-point v2))))

(define (sub-vector v1 v2)
  (make-point (- (x-point v1) (x-point v2))
              (- (y-point v1) (y-point v2))))

(define (orthogonal? v1 v2)
  (= 0.0 (dot-product v1 v2))) ; should it be better with an small range?

; Constructor
(define (make-rect p1 p2 p3)
  (if (orthogonal? (sub-vector p2 p1) (sub-vector p3 p1))
      (cons p1 (cons p2 p3))
      (error "Points should make an rectangle."))) ; check orthogonality, which is longer? (error "Argument not 0 or 1: CONS" m)

(define (p1-rect r) (car  r))
(define (p2-rect r) (car (cdr r)))
(define (p3-rect r) (cdr (cdr r)))

; Public interface
(define (height-rect r) (distance-point (p1-rect r) (p2-rect r)))
(define (width-rect  r) (distance-point (p1-rect r) (p3-rect r)))

;; testng
(define p4 (make-point 0 0))
(define p5 (make-point 10 -2))
(define p6 (make-point 1 5))

(define r2 (make-rect p4 p5 p6))

(display "Rectangle 2: ") (newline)
(display "Perimeter: ") (display (perimeter-rect r2)) (newline)
(display "Area ") (display (area-rect r2)) (newline) (newline)
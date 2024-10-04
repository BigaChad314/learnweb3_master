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

; Looks like vectors...
(define (dot-product p1 p2)
  (+ (* (x-point p1) (x-point p2))
     (* (y-point p1) (y-point p2))))

(define (add-vector v1 v2)
  (make-point (+ (x-point v1) (x-point v2))
              (+ (y-point v1) (y-point v2))))

(define (sub-vector v1 v2)
  (make-point (- (x-point v1) (x-point v2))
              (- (y-point v1) (y-point v2))))

;; ---
(define (make-rect origin height width angle)
  (cons (cons height width) (cons origin angle)))

(define (origin-rect r) (car (cdr r)))
(define (angle-rect  r) (cdr (cdr r)))

; Public interface
(define (height-rect r) (car (car r)))
(define (width-rect  r) (cdr (car r)))


(define origin1 (make-point 1 1))
(define height1 4.0)
(define width1 5.0)
(define angle1 0.2)

(define r1 (make-rect origin1 height1 width1 angle1))

(display "Rectangle 1: ") (newline)
(display "Perimeter: ") (display (perimeter-rect r1)) (newline)
(display "Area ") (display (area-rect r1)) (newline) (newline)
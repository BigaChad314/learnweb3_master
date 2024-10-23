#lang racket

(define (make-vect x y)
    (cons x y))

(define (xcor-vect v)
    (car v))

(define (ycor-vect v)
    (cdr v))

(define (add-vect v1 v2)
    (make-vect (+ (xcor-vect v1) (xcor-vect v2))
                (+ (ycor-vect v1) (ycor-vect v2))
    ))

(define (scale-vect v s)
    (make-vect (* s (xcor-vect v))
                (* s (ycor-vect v))
    ))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (cadr (cdr f)))

(define (make-segment start-vect end-vect)
  (cons start-vect end-vect))

(define smile-segments
  (list (make-segment (make-vect 0.46 0.13) (make-vect 0.46 0.17))
        (make-segment (make-vect 0.46 0.24) (make-vect 0.50 0.27))
        (make-segment (make-vect 0.54 0.13) (make-vect 0.54 0.17))
        (make-segment (make-vect 0.54 0.24) (make-vect 0.50 0.27))))
(define wave
  (segments->painter (append wave-segments smile-segments)))
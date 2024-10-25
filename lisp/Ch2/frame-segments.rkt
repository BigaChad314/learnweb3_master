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

(define (sub-vect v1 v2)
    (make-vect (- (xcor-vect v1) (xcor-vect v2))
                (- (ycor-vect v1) (ycor-vect v2))
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

(define (draw-line p1 p2)
  (display (format "Line from ~s to ~s\n" p1 p2)))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line ((frame-coord-map frame)
                   (start-segment segment))
                  ((frame-coord-map frame)
                   (end-segment segment))))
     segment-list)))
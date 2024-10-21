#lang racket

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

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (segments->painter segment-list)
    (lambda (frame)
        (for-each
        (lambda (segment)
            (draw-line
                ((frame-coord-map frame) (start-segment segment))
                ((frame-coord-map frame) (end-segment segment))))
        segment-list)))
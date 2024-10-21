#lang racket

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (cadr (cdr f)))
  
(define (segments->painter segment-list)
    (lambda (frame)
        (for-eac
        (lambda (segment)
            (draw-line
                ((frame-coord-map frame) (start-segment segment))
                ((frame-coord-map frame) (end-segment segment))
        segment-list)))))
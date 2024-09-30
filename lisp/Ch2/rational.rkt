#lang racket

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; --- Solution ---

;; 원래 ver. sign이 맨 앞으로 오지 않는다. (ex. 1/-2)
;; (define (make-rat n d)
;;     (let ((g(gcd n d)))
;;     (cons (/ n g) (/ d g))))

(define (make-rat n d)
   (let ((g (gcd n d))
         (denom-sign (if (> d 0) 1 (- 1))))
     (cons (* (/ n g) denom-sign)
           (* (/ d g) denom-sign))))

; --- Tests ---

(print-rat (make-rat  2 -4))
(print-rat (make-rat -2  4))
(print-rat (make-rat 3  4))
#lang racket

(define (make-mobile left right) 
        (list left right))

(define (make-branch length structure) 
        (list length structure))

;; Part a. branch, branch-length, branch-structure
(define (left-branch x)
  (car x))

(define (right-branch x)
  (car (cdr x)))

(define (branch-length x)
  (car x))

(define (branch-structure x)
  (car (cdr x)))
;;          4  |  5 
;;        +----+-----+ 
;;        6        3 |     9 
;;               +---+---------+ 
;;               7             8 
(define m1 (make-mobile 
            (make-branch 4 6) 
            (make-branch 5 
                         (make-mobile 
                          (make-branch 3 7) 
                          (make-branch 9 8)))))

;;          4  | 2 
;;        +----+--+ 
;;        6    5  |    10 
;;          +-----+----------+ 
;;          8                4 
(define m2 (make-mobile 
            (make-branch 4 6) 
            (make-branch 2 
                         (make-mobile 
                          (make-branch 5 8) 
                          (make-branch 10 4)))))


;; Part b. Total weight

(define (mobile-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (number? struct)
        struct
        (mobile-weight struct))))

;; Part c. Balance
(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (mobile-balanced? mobile)
  (and (= (torque (left-branch mobile))
          (torque (right-branch mobile)))
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))))

(define (branch-balanced? branch)
  (let ((struct (branch-structure branch)))
    (or (number? struct)
        (mobile-balanced? struct))))

;; Part d. Constructor
;; (mobile-weight m1)
;; (mobile-weight m2)
;; (balanced? m1)
;; (balanced? m2)

;; Test
(mobile-weight m1)
(mobile-weight m2)
(mobile-balanced? m1)
(branch-balanced? m2)
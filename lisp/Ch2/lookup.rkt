#lang racket

(define make-tree list)
(define entry car)
(define left-branch cadr)
(define right-branch caddr)

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define key car)

(lookup 2 '((1 flour) (2 water) (3 salt)))
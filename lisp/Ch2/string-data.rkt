#lang racket

(list 'a 'b 'c)

(list (list 'george))

(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))

(memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))

(define (equal? list1 list2)
  (cond ((null? list1) (null? list2))
        ((not (pair? list1)) (eq? list1 list2))
        (else (and (pair? list2)
                   (equal? (car list1) (car list2))
                   (equal? (cdr list1) (cdr list2))))))

(equal? '(this is a list) '(this is a list))
(equal? '(this (is a) list) '(this (is a) list))
(equal? '(this is a list) '(this (is a) list))

(car ''abracadabra)
#lang racket/base
;; 비록 divisor.rkt의 find-divisor보다 검사 횟수가 반으로 줄었지만, if (= n 2)와 추가 function call로 인해 2배보다 덜빠르다.

(define (square x) (* x x))

(define (next n)
  (if (= n 2)
      3
      (+ 2 n)))

(define (smallest-divisor n)
    (find-divisor-improved n 2))

(define (find-divisor-improved n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor-improved
               n
               (next test-divisor)))))

(define (divides? a b)
    (= (remainder b a) 0))

(smallest-divisor 199989)
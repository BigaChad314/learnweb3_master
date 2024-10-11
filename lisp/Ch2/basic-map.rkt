#lang racket

(define (square x)
  (* x x))  ; x의 제곱을 계산하는 함수

(define (map proc items)
  (if (null? items)
      empty
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list items)
  (if (null? items)
      empty
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list2 items)
  (map square items))

(square-list (list 1 2 3 4))
(square-list2 (list 1 2 3 4))

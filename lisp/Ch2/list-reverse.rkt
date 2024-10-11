#lang racket

;; // Iterative solution
;; (define (reverse xs)
;;   (define (iter xs ys)
;;     (if (null? xs)
;;         ys
;;         (iter (cdr xs)
;;               (cons (car xs) ys))))
;;   (iter xs '()))

(define (reverse lst)
  (if (null? lst)          ; 리스트가 빈 경우 그대로 반환
      '()                  ; 빈 리스트 반환
      (append (reverse (cdr lst)) (list (car lst)))))  ; 재귀적으로 호출하여 뒤집음

(reverse (list 1 4 9 16 25))
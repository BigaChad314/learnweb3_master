#lang racket

(define x (list (list 1 2) (list 3 4)))

(define (fringe items)
  (cond ((null? items) '())                 ; 리스트가 비어있다면 빈 리스트를 반환
        ((pair? (car items))                ; 리스트의 첫 번째 요소가 또 다른 리스트라면
         (append (fringe (car items))       ; 첫 번째 리스트를 재귀적으로 처리하고
                 (fringe (cdr items))))     ; 나머지 리스트도 재귀적으로 처리
        (else (cons (car items) 
                    (fringe (cdr items))))))  ; 첫 번째 요소가 리스트가 아니라면 결과에 추가


(display x)
(newline)
(fringe x)
(newline)
(fringe (list x x))
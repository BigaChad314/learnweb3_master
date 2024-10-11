#lang racket

(define (same-parity first . rest)
  (define (parity n) 
    (= (remainder n 2) (remainder first 2)))  ; 첫 번째 인자의 짝/홀 여부와 같은지 확인

  (cons first (filter parity rest)))  ; 조건에 맞는 나머지 요소를 필터링하여 리스트 생성

;; 테스트 예시
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7 8)

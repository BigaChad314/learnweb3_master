#lang racket

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 1/2))

;; // Original ver.
;; (define (cc amount kinds-of-coins)
;;     (cond ((= amount 0) 1)
;;         ((or (< amount 0) (= kinds-of-coins 0)) 0)
;;         (else (+ (cc amount
;;                     (- kinds-of-coins 1))
;;                 (cc (- amount
;;                         (first-denomination kinds-of-coins))
;;                         kinds-of-coins)))))

;; // New ver. 동전 값어치의 리스트를 받을 수 있게끔 수정
(define (cc amount coin-values)
    (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else 
            (+ (cc amount
                    (except-first-denomination coin-values))
                (cc (- amount
                        (first-denomination coin-values))
                        coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(cc 100 us-coins)
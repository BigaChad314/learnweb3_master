#lang racket

;; Basis: Accumulate 
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define make-position list)
(define get-row car)
(define get-col cadr)

(define empty-board '())
(define (adjoin-position row col board)
  (cons (make-position row col) board))

(define (safe? positions)
  (let ((row1 (get-row (car positions))))
    (define (helper rest-of-queens cols-apart)
      (or (null? rest-of-queens)
          (let ((row2 (get-row (car rest-of-queens))))
            (and (not (= row1 row2))
                 (not (= row1 (- row2 cols-apart)))
                 (not (= row1 (+ row2 cols-apart)))
                 (helper (cdr rest-of-queens) (+ cols-apart 1))))))
    (helper (cdr positions) 1)))

(define (queen-cols k board-size)
  (if (= k 0)
      (list empty-board)
      (filter safe?
              (flatmap
               (lambda (rest-of-queens)
                 (map (lambda (new-row)
                        (adjoin-position new-row k rest-of-queens))
                      (enumerate-interval 1 board-size)))
               (queen-cols (- k 1) board-size)))))

(define (queens board-size)
  (queen-cols board-size board-size))

(queens 0)
(queens 4)
(car (queens 8))
#lang racket

(define make-tree list)
(define entry car)
(define left-branch cadr)
(define right-branch caddr)

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '())) 

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (this-entry (car non-left-elts))
             (right-size (- n (+ left-size 1)))
             (right-result (partial-tree (cdr non-left-elts) right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry left-tree right-tree)
              remaining-elts))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                 (else (cons x2 (union-set set1 (cdr set2)))))))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((> x1 x2)
               (intersection-set set1 (cdr set2)))))))
              
(define (union-tree set1 set2)
  (list->tree (union-set (tree->list set1) (tree->list set2))))

(define (intersection-tree set1 set2)
  (list->tree (intersection-set (tree->list set1) (tree->list set2))))

(define s1 '(5 (3 (1 () ())
                  ())
               (9 (7 () ())
                  (11 () ()))))

(define s2 '(8 (6 (4 () ())
                  ())
               (11 (9 () ())
                   (13 () ()))))

(union-tree s1 s2)
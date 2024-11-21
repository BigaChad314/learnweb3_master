#lang racket

;; pre-req
(define (using . installers)
  (reset)
  (for-each (lambda (f) (f)) installers))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record (cdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table))))))
    (define (reset!)
      (set-cdr! local-table '()))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'reset-proc!) reset!)
            (else (error 'make-table "unknown operation" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error 'type-tag "bad tagged datum" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error 'contents "bad tagged datum" datum))))

(attach-tag 'foo 'a) 
(attach-tag 'scheme-number 1) 
(type-tag '(foo . a)) 
(type-tag 1) 
(contents '(foo . a)) 
(contents 1) 

(paste (:2.4.3 apply-generic) (:2.5.1 add div mul scheme-number-pkg sub))

(using scheme-number-pkg)

(add 1 2) 
(mul 3 4) 
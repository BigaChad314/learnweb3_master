#lang racket

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))
        (else (apply-specific 'deriv (operator expr) (operands expr) var))))
(define operator car)
(define operands cdr)

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? expr num)
  (and (number? expr) (= expr num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        (else (list '** b e))))


(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error 'type-tag "bad tagged datum" datum)))

;; get and put
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))
;; 

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (apply-specific op type . args)
  (let ((proc (get op type)))
    (if proc
        (apply proc args)
        (error op "no method for type" op type))))

;; using package for operation
(define (sum-pkg)
  (define (deriv-sum terms var)
    (accumulate make-sum 0 (map (lambda (t) (deriv t var)) terms)))
  (put 'deriv '+ deriv-sum))

(define (product-pkg)
  (define multiplier car)
  (define (multiplicand product)
    (accumulate make-product 1 (cdr product)))
  (define (deriv-product product var)
    (make-sum (make-product (multiplier product)
                            (deriv (multiplicand product) var))
              (make-product (deriv (multiplier product) var)
                            (multiplicand product))))
  (put 'deriv '* deriv-product))

(define (power-pkg)
  (define base car)
  (define exponent cadr)
  (define (deriv-power power var)
    (make-product
     (make-product (exponent power)
                   (make-exponentiation
                    (base power)
                    (make-sum (exponent power) -1)))
     (deriv (base power) var)))
  (put 'deriv '** deriv-power))

(using sum-pkg product-pkg power-pkg)

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x) 
(deriv '(* (* x y) (+ x 3)) 'x) 
(deriv '(* 3 (** x 5)) 'x)
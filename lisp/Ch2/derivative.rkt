#lang racket

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum (make-product (multiplier expr)
                                 (deriv (multiplicand expr) var))
                   (make-product (deriv (multiplier expr) var)
                                 (multiplicand expr))))
        ((exponentiation? expr)
         (make-product
          (exponent expr)
          (make-product (make-exponentiation (base expr) (- (exponent expr) 1))
                        (deriv (base expr) var))))
        (else (error 'deriv "unknown expr type" expr))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; (define (make-sum a1 a2) (list '+ a1 a2))
;; (define (make-product m1 m2) (list '* m1 m2))

;; For more condesned answer. 값이 상수라면 list가 아닌, 두 값을 계산한 값을 반환.
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

(define (exponentiation? expr)
  (and (pair? expr) (eq? (car expr) '**)))
(define base cadr)
(define exponent caddr)

(define (sum? expr)
  (and (pair? expr) (eq? (car expr) '+)))

(define addend cadr)
(define augend caddr)

(define (product? expr)
  (and (pair? expr) (eq? (car expr) '*)))
(define multiplier cadr)
(define multiplicand caddr)

(deriv '(+ x 3) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* 3 (** x 5)) 'x)
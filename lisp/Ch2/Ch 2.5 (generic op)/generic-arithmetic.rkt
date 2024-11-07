#lang racket

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (scheme-number-pkg)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number tag))

(define (make-scheme-number n)
  (apply-specific 'make 'scheme-number n))

(define (rational-pkg)
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d)))))

(define (make-rational n d)
  (apply-specific 'make 'rational n d))

(define (complex-pkg)
  (define (tag z) (attach-tag 'complex z))
  (rectangular-pkg)
  (polar-pkg)
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a)))))

(define (make-complex-from-real-imag x y)
  (apply-specific 'make-from-real-imag 'complex x y))
(define (make-complex-from-mag-ang r a)
  (apply-specific 'make-from-mag-ang 'complex r a))

(define (numeric-pkg)
  (scheme-number-pkg)
  (rational-pkg)
  (complex-pkg))

(using numeric-pkg)

(add (make-scheme-number 1) (make-scheme-number 2))

(mul (make-rational 1 2) (make-rational 3 4))

(sub (make-complex-from-mag-ang 1 0) (make-complex-from-real-imag 1 1))
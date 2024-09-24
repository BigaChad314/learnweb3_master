;; From the BOOK
;; (defun smallest-divisor (n)
;;   (find-divisor n 2))

;; (defun find-divisor (n test-divisor)
;;   (cond ((> (* test-divisor test-divisor) n) n)
;;         ((divides? test-divisor n) test-divisor)
;;         (t (find-divisor n (+ test-divisor 1)))))

;; (defun divides? (a b)
;;   (= (mod b a) 0))

(defun square (x)
  (* x x))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (t (find-divisor n (+ test-divisor 1)))))

(defun divides? (a b)
  (= (mod b a) 0))

(trace find-divisor)

(smallest-divisor 19999)

(defun (cube x) (* x x x))

(defun (good-enough? previous-guess guess)
  (< (abs (/ (- guess previous-guess) guess)) 0.00000000001))

(defun (cube-root-iter guess x)
  (if (good-enough? (improve guess x) guess)
      guess
      (cube-root-iter (improve guess x) x)))

(defun (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))


(defun (cube-root x)
  (cube-root-iter 1.0 x))

;; ; Basic testing
;; (defun x 12345)
;; (defun cube-root-x (cube-root x))
;; (newline)
;; (display "(cube-root ") (display x) (display ") -> ") (display cube-root-x)
;; (newline)
;; (display "(cube ") (display cube-root-x) (display ") -> ") (display (cube cube-root-x))

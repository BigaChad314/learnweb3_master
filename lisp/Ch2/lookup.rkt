#lang racket

(define make-tree list)
(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define key car)

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        (else
         (let* ((record (entry set-of-records))
                (rec-key (key record)))
           (cond ((= given-key rec-key) record)
                 ((< given-key rec-key)
                  (lookup given-key (left-branch set-of-records)))
                 ((> given-key rec-key)
                  (lookup given-key (right-branch set-of-records))))))))

(lookup 3 '((2 water) ((1 flour) () ()) ((3 salt) () ())))

;; let 과 let* 의 차이점:
;; let: 모든 변수를 동시에 묶습니다. 즉, 각 변수가 정의될 때 다른 변수에 의존하지 않고, 별도의 평가 없이 바로 정의됩니다. 
;; 예를 들어 (let ((a 3) (b a)))와 같이 작성하면 a가 먼저 정의되지 않았으므로 오류가 발생합니다.

;; let*: 각 변수를 순차적으로 묶습니다. 첫 변수를 정의한 후, 다음 변수는 이전 변수를 참조할 수 있게 됩니다. 
;; 따라서 (let* ((a 3) (b a)))와 같이 작성하면 a가 정의된 후 b에서 참조가 가능해 오류가 발생하지 않습니다.
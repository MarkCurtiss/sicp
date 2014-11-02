; 5.23
; ========================================================================
;; I added the following to eceval
;;   (test (op cond?) (reg exp))
;;   (branch (label ev-cond))
;; ....
;; ev-cond
;;   (assign exp (op cond->if) (reg exp))
;;   (goto (label eval-dispatch))

(define x 9)
(cond ((> x 0) x)
      ((= x 0) (display zero) 0)
      (else (- x)))

;;; EC-Eval input:
(define x 9)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(cond ((> x 0) x)
		   ((= x 0) (display zero) 0)
		   (else (- x)))

(total-pushes = 11 maximum-depth = 8)
;;; EC-Eval value:
9

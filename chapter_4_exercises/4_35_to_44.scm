;; Note: I haven't really figured out a great way to store my solutions
;; to these problems.  I can't use our unit testing framework 'cuz all
;; of these problems require you to run a Scheme sub-evaluator.  For the
;; same reason, I can't leave my code uncommented - it'd fail to compile
;; if you loaded it into an evaluator.

;; So I'm just leaving big blobs of comments that showed what I fed into
;; the sub-evaluator and its output.

; 4.35
; ========================================================================
;; (load "book_code/ch4-ambeval.scm")
;; (define the-global-environment (setup-environment))
;; (driver-loop)

;;; Amb-Eval input:
;; (define (require p) (if (not p) (amb)))
;;; Starting a new problem
;;; Amb-Eval value:
;; ok

;;; Amb-Eval input:
;; (define (an-integer-between low high)
;;   (require (> high low))
;;   (amb low (an-integer-between (+ 1 low) high)))
;;; Starting a new problem
;;; Amb-Eval value:
;; ok

;;; Amb-Eval input:
;; (an-integer-between 3 6)

;;; Starting a new problem
;;; Amb-Eval value:
;; 3

;;; Amb-Eval input:
;; try-again

;;; Amb-Eval value:
;; 4

;;; Amb-Eval input:
;; try-again

;;; Amb-Eval value:
;; 5

;;; Amb-Eval input:
;; try-again

;;; There are no more values ofI
;; (an-integer-between 3 6)

; 4.36
; ========================================================================
;; ;;; Amb-Eval input:
;; (define (an-integer-starting-from n)
;;   (amb n (an-integer-starting-from (+ n 1))))

;; ;; ;;; Starting a new problem
;; ;; ;;; Amb-Eval value:
;; ;; ok

;; ;; With no upper bound on k the evaluator will keep generating integers
;; ;; forever.  Thus it never backtracks to try generating new values for j and i.
;; (define (a-pythagorean-triple)
;;   (let ((k (an-integer-starting-from 1)))
;;     (let ((j (an-integer-between 1 k)))
;;       (let ((i (an-integer-between 1 j)))
;;         (require (= (+ (* i i) (* j j)) (* k k)))
;;         (list i j k)))))

;; ;;; Amb-Eval input:
;; (a-pythagorean-triple)

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; (3 4 5)

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; (6 8 10)

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; (5 12 13)

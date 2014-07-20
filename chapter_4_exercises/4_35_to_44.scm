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

; 4.37
; ========================================================================
;; Yes Ben's implementation should be more efficient.  He only has to explore
;; at max (high - low)^2 possibilities while the previous implementation
;; had to explore (high - low)^3 possibilities.


; 4.38
; ========================================================================
;; (define (distinct? items)
;;   (cond ((null? items) true)
;;         ((null? (cdr items)) true)
;;         ((member (car items) (cdr items)) false)
;;         (else (distinct? (cdr items)))))

;; (define (multiple-dwelling)
;;   (let ((baker (amb 1 2 3 4 5))
;;         (cooper (amb 1 2 3 4 5))
;;         (fletcher (amb 1 2 3 4 5))
;;         (miller (amb 1 2 3 4 5))
;;         (smith (amb 1 2 3 4 5)))
;;     (require
;;      (distinct? (list baker cooper fletcher miller smith)))
;;     (require (not (= baker 5)))
;;     (require (not (= cooper 1)))
;;     (require (not (= fletcher 5)))
;;     (require (not (= fletcher 1)))
;;     (require (> miller cooper))
;;     (require (not (= (abs (- fletcher cooper)) 1)))
;;     (list (list 'baker baker)
;;           (list 'cooper cooper)
;;           (list 'fletcher fletcher)
;;           (list 'miller miller)
;;           (list 'smith smith))))

;; ;;; Amb-Eval input:
;; (define (distinct? items)
;;   (cond ((null? items) true)
;;         ((null? (cdr items)) true)
;;         ((member (car items) (cdr items)) false)
;;         (else (distinct? (cdr items)))))

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok

;; ;;; Amb-Eval input:
;; (define (multiple-dwelling)
;;   (let ((baker (amb 1 2 3 4 5))
;;         (cooper (amb 1 2 3 4 5))
;;         (fletcher (amb 1 2 3 4 5))
;;         (miller (amb 1 2 3 4 5))
;;         (smith (amb 1 2 3 4 5)))
;;     (require
;;      (distinct? (list baker cooper fletcher miller smith)))
;;     (require (not (= baker 5)))
;;     (require (not (= cooper 1)))
;;     (require (not (= fletcher 5)))
;;     (require (not (= fletcher 1)))
;;     (require (> miller cooper))
;;     (require (not (= (abs (- fletcher cooper)) 1)))
;;     (list (list 'baker baker)
;;           (list 'cooper cooper)
;;           (list 'fletcher fletcher)
;;           (list 'miller miller)
;;           (list 'smith smith))))

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok

;; ;;; Amb-Eval input:
;; (multiple-dwelling)

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; ((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; ((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; There are no more values of
;; (multiple-dwelling)

;; There are 5 solutions to the modified version of (multiple-dwellings).

; 4.39
; ========================================================================
;; The order of the restrictions doesn't affect the answer.  It does
;; affect the runtime of the program.  If you put more restrictive checks
;; before less restrictive ones you should be able to cut down on your
;; problem search space.

;;; Amb-Eval input:
;; (define (multiple-dwelling)
;;   (let ((baker (amb 1 2 3 4 5))
;;         (cooper (amb 1 2 3 4 5))
;;         (fletcher (amb 1 2 3 4 5))
;;         (miller (amb 1 2 3 4 5))
;;         (smith (amb 1 2 3 4 5)))
;;     (require
;;      (distinct? (list baker cooper fletcher miller smith)))
;;     (require (> miller cooper))
;;     (require (not (= (abs (- smith fletcher)) 1)))
;;     (require (not (= (abs (- fletcher cooper)) 1)))
;;     (require (not (= baker 5)))
;;     (require (not (= cooper 1)))
;;     (require (not (= fletcher 5)))
;;     (require (not (= fletcher 1)))
;;     (list (list 'baker baker)
;;           (list 'cooper cooper)
;;           (list 'fletcher fletcher)
;;           (list 'miller miller)
;;           (list 'smith smith))))

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok

;; ;;; Amb-Eval input:
;; (multiple-dwelling)

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; There are no more values of
;; (multiple-dwelling)

;; ;; However on my comically fast computer this doesn't seem to run any
;; ;; 'slower' or 'faster' than the original.


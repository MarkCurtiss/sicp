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

; 4.40
; ========================================================================
;; With no restriction on floor assignments there are 5^5 possible
;; solutions to this problem.
;; With restrictions on floor assignments there are 5! possible solutions.

;; This version runs noticably faster by restricting each subproblem
;; to valid combinations from the previous problem.
;;; Amb-Eval input:
;; (define (multiple-dwelling)
;;   (let ((cooper (amb 1 2 3 4 5)))
;;     (require (not (= cooper 1)))
;;     (let ((miller (amb 1 2 3 4 5)))
;;       (require (> miller cooper))
;;       (let ((fletcher (amb 1 2 3 4 5)))
;; 	    (require (not (= fletcher 5)))
;; 	    (require (not (= fletcher 1)))
;; 	    (require (not (= (abs (- fletcher cooper)) 1)))
;; 	    (let ((baker (amb 1 2 3 4 5)))
;; 		  (require (not (= baker 5)))
;; 		  (let ((smith (amb 1 2 3 4 5)))
;; 		        (require (not (= (abs (- smith fletcher)) 1)))
;; 			(require
;; 			 (distinct? (list baker cooper fletcher miller smith)))
;; 			(list (list 'baker baker)
;; 			      (list 'cooper cooper)
;; 			      (list 'fletcher fletcher)
;; 			      (list 'miller miller)
;; 			      (list 'smith smith))))))))

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok

;; ;;; Amb-Eval input:
;; (multiple-dwelling)

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

; 4.41
; ========================================================================
;; Ha ha this version tries combinations randomly until it finds a
;; working solution.  It actually converged quickly on a solution!

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (define (valid-solution? baker cooper fletcher miller smith)
    (if (and
	 (not (= cooper 1))
	 (> miller cooper)
	 (not (= fletcher 5))
	 (not (= fletcher 1))
	 (not (= (abs (- fletcher cooper)) 1))
	 (not (= baker 5))
	 (not (= (abs (- smith fletcher)) 1))
	 (distinct? (list baker cooper fletcher miller smith)))
	#t
	#f))

  (define (rand5) (+ (random 5) 1))

  (define (iter baker cooper fletcher miller smith)
    (if (valid-solution? baker cooper fletcher miller smith)
	(list (list 'baker baker)
	      (list 'cooper cooper)
	      (list 'fletcher fletcher)
	      (list 'miller miller)
	      (list 'smith smith))
	(iter (rand5) (rand5) (rand5) (rand5) (rand5))))

  (iter 1 1 1 1 1))


;; 1 ]=> (multiple-dwelling)
;; Value 4: ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

; 4.42
; ========================================================================
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
;; (define (require p) (if (not p) (amb)))

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok

;; ;;; Amb-Eval input:
;; (define (liars)
;;   (let ((betty (amb 1 3))
;; 	(ethel (amb 1 5))
;; 	(joan (amb 2 3))
;; 	(kitty (amb 2))
;; 	(mary (amb 4)))
;;     (require
;;      (distinct? (list betty ethel joan kitty mary)))
;;     (list (list 'betty betty)
;; 	  (list 'ethel ethel)
;; 	  (list 'joan joan)
;; 	  (list 'kitty kitty)
;; 	  (list 'mary mary))))

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok

;; ;;; Amb-Eval input:
;; (liars)

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ((betty 1) (ethel 5) (joan 3) (kitty 2) (mary 4))

;; Betty, Kitty, Joan, Mary, Ethel.

; 4.43
; ========================================================================
;; ;;; Amb-Eval input:
;; (define (require p) (if (not p) (amb)))

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok

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
;; (define (lornas-father)
;;   (let ((mr-moore "Mr Moore")
;; 	(col-downing "Colonel Downing")
;; 	(mr-hall "Mr Hall")
;; 	(sir-barnacle-hood "Sir Barnacle Hood")
;; 	(dr-parker "Dr Parker"))
;;     (let ((mary (amb mr-moore))
;; 	  (melissa (amb sir-barnacle-hood))
;; 	  (lorna (amb col-downing mr-hall dr-parker))
;; 	  (rosalind (amb col-downing dr-parker))
;; 	  (gabrielle (amb col-downing mr-hall)))

;;       (require
;;        (distinct? (list gabrielle lorna rosalind melissa mary)))

;;       (list (list 'Gabrielle gabrielle)
;; 	    (list 'Lorna lorna)
;; 	    (list 'Rosalind rosalind)
;; 	    (list 'Melissa melissa)
;; 	    (list 'Mary mary)))))

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok

;; ;;; Amb-Eval input:
;; (lornas-father)

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ((gabrielle Mr Hall) (lorna Colonel Downing) (rosalind Dr Parker) (melissa Sir Barnacle Hood) (mary Mr Moore))


;; ;; ;; If we don't know that Mary is Mr. Moore's daughter. . .
;; ;;; Amb-Eval input:
;; (define (lornas-father-mary-isnt-moore)
;;   (let ((mr-moore "Mr Moore")
;; 	(col-downing "Colonel Downing")
;; 	(mr-hall "Mr Hall")
;; 	(sir-barnacle-hood "Sir Barnacle Hood")
;; 	(dr-parker "Dr Parker"))
;;     (let ((melissa (amb sir-barnacle-hood))
;; 	  (lorna (amb col-downing mr-hall dr-parker))
;; 	  (mary (amb mr-moore col-downing mr-hall dr-parker))
;; 	  (rosalind (amb mr-moore col-downing dr-parker))
;; 	  (gabrielle (amb mr-moore col-downing mr-hall)))
;;       (require
;;        (distinct? (list gabrielle lorna rosalind melissa mary)))
;;       (require
;;        (cond ((eq? gabrielle mr-moore) (eq? lorna dr-parker))
;; 	     ((eq? gabrielle mr-hall) (eq? rosalind dr-parker))
;; 	     (else false)))

;;       (list (list 'Gabrielle gabrielle)
;; 	    (list 'Lorna lorna)
;; 	    (list 'Rosalind rosalind)
;; 	    (list 'Melissa melissa)
;; 	    (list 'Mary mary)))))

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok

;; ;;; Amb-Eval input:
;; (lornas-father-mary-isnt-moore)

;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ((gabrielle Mr Hall) (lorna Colonel Downing) (rosalind Dr Parker) (melissa Sir Barnacle Hood) (mary Mr Moore))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; ((gabrielle Mr Moore) (lorna Dr Parker) (rosalind Colonel Downing) (melissa Sir Barnacle Hood) (mary Mr Hall))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; There are no more values of
;; (lornas-father-mary-isnt-moore)
; 5.7
; ========================================================================
;; See tests.

; 5.8
; ========================================================================
;; (load "book_code/ch5-regsim.scm")
;; (define ambiguous-machine
;;   (make-machine
;;    '(a)
;;    (list '())
;;    '(
;;      start
;;      (goto (label here))
;;      here
;;      (assign a (const 3))
;;      (goto (label there))
;;      here
;;      (assign a (const 4))
;;      (goto (label there))
;;      there
;;      ))
;;   )

;; (start ambiguous-machine)
;; (get-register-contents ambiguous-machine 'a)

;; 1 ]=>
;; Value: 3

;; For my fixes to detect redundant labels, see book_code/ch5-regsim.scm
;; inside of (extract-labels):
;; (if (memq next-inst (map car labels))
;;   (error "Redundant label detected:" next-inst)

; 5.9
; ========================================================================
;; See my changes to book_code/ch5-regsim.scm
;; In (make-operation-exp):
;; (if (list-search-positive (operation-exp-operands exp) label-exp?)
;;     (error "Attempted to apply an operation to a label -- MAKE-OPERATION-EXP")

; 5.10
; ========================================================================
;; Nice try, SICP!  But once bitten, twice shy.  If I change the syntax
;; for this evaluator we've built up I just now it is going to cause
;; problems down the road: you'll ask me to do some other change that is
;; incompatible with the new syntax.  Or you'll give me code to run that uses
;; the existing syntax.  I'm going to make my changes to a separate interpreter
;; defined in alternate-syntax-regsim.scm.  See the accompanying tests.

; 5.11
; ========================================================================
;; a.
(load "book_code/ch5-regsim.scm")
(define fib-machine
  (make-machine
   '(n val continue)
   (list
    (list '< <)
    (list '- -)
    (list '+ +))
   '(
     (assign continue (label fib-done))
    fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n - 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)                           ; save old value of n
     (assign n (op -) (reg n) (const 1)); clobber n to n - 1
     (goto (label fib-loop))            ; perform recursive call
    afterfib-n-1                         ; upon return, val contains Fib(n - 1)
     (restore n)
     (restore continue)
     ;; set up to compute Fib(n - 2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)                         ; save Fib(n - 1)
     (goto (label fib-loop))
    afterfib-n-2                         ; upon return, val contains Fib(n - 2)
     (restore n)
     (restore continue)
     (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
	     (op +) (reg val) (reg n))
     (goto (reg continue))              ; return to caller, answer is in val
    immediate-answer
     (assign val (reg n))               ; base case:  Fib(n) = n
     (goto (reg continue))
     fib-done)
   ))

(set-register-contents! fib-machine 'n 6)
(start fib-machine)
(get-register-contents fib-machine 'val)

;; In (afterfib-n-2) I replaced
;; (assign n (reg val))
;; (restore val)
;; with
;; (restore n)
;; Which puts the value of (fib (n - 2)) into n
;; since it was saved there up in fib-loop before goto'ing to
;; afterfib-n-2
;; 1 ]=>
;Value: 8

;; b.
;;Check out named-stack-regsim.scm as well as the associated test.

;; c.
;; See multi-stack-register-machine.scm and the associated test.

; 5.12
; ========================================================================
;; a. b. c.
;; See reflective-register-machine.scm and its associated tests.

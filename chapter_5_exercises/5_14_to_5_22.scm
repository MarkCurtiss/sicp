; 5.14
; ========================================================================
(load "book_code/ch5-regsim.scm")

(define factorial-machine
  (make-machine
   '(n continue val)
   (list
    (list '= =)
    (list '- -)
    (list '* *))
   '((perform (op initialize-stack))
     (assign continue (label fact-done))
    fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
    after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))
     (goto (reg continue))
    base-case
     (assign val (const 1))
     (goto (reg continue))
    fact-done
     (perform (op print-stack-statistics)))
   ))

(set-register-contents! factorial-machine 'n 4)
(start factorial-machine)

;; 1 ]=>
;; (total-pushes = 6 maximum-depth = 6)
;Value: done

(set-register-contents! factorial-machine 'n 2)
(start factorial-machine)

;; 1 ]=>
;; (total-pushes = 2 maximum-depth = 2)
;Value: done

(set-register-contents! factorial-machine 'n 3)
(start factorial-machine)

;; 1 ]=>
;; (total-pushes = 4 maximum-depth = 4)
;Value: done

(set-register-contents! factorial-machine 'n 6)
(start factorial-machine)

;; 1 ]=>
;; (total-pushes = 10 maximum-depth = 10)
;Value: done

;; The stack pushes seem to grow by 2 for every increase in the value of N.
;; So stack_ops == 2*n - 2

; 5.15
; ========================================================================
;; See tests, and my changes to book_code/ch5-regsim.scm
;; The machine stores an instruction count that gets incremented during
;; (execute) and reset during (start).

; 5.16
; ========================================================================
;; See my changes to ch5-regsim.scm and the associated test.
;; I added the instruction tracing to (execute) in the machine.

; 5.17
; ========================================================================
;; Okay I updated the existing test and the implementation for ch5-regsim.
;; Man the implementation of this was so grafted on that I am considering
;; creating a separate register machine just for this problem.  I 'feel'
;; like it will wind up causing compatibility issues in the future.  I
;; had to essentially manufacture a new instruction type that is only
;; ever displayed but not executed, but inject it into the same instruction
;; stream that the rest of the machine uses.

;; Maybe a better way to do it would have been to actually change the label
;; expression type to have a '() execution proc.

; 5.18
; ========================================================================
;; See my changes to (make-register) in ch5-regsim.scm as well as the
;; test "it can trace register assignments"

; 5.19
; ========================================================================
(load "book_code/ch5-regsim.scm")

;; (define factorial-machine
;;   (make-machine
;;    '(n continue val)
;;    (list
;;     (list '= =)
;;     (list '- -)
;;     (list '* *))
;;    '((perform (op initialize-stack))
;;      (assign continue (label fact-done))
;;     fact-loop
;;      (test (op =) (reg n) (const 1))
;;      (branch (label base-case))
;;      (save continue)
;;      (save n)
;;      (assign n (op -) (reg n) (const 1))
;;      (assign continue (label after-fact))
;;      (goto (label fact-loop))
;;     after-fact
;;      (restore n)
;;      (restore continue)
;;      (assign val (op *) (reg n) (reg val))
;;      (goto (reg continue))
;;     base-case
;;      (assign val (const 1))
;;      (goto (reg continue))
;;     fact-done
;;    )))

;; (set-register-contents! factorial-machine 'n 4)
;; (set-breakpoint factorial-machine 'fact-loop 5)
;; (enable-instruction-tracing factorial-machine)
;; (start factorial-machine)
;; ------OUTPUT------
;; 1 ]=>
;; (perform (op initialize-stack))
;; (assign continue (label fact-done))
;; fact-loop
;; (test (op =) (reg n) (const 1))
;; (branch (label base-case))
;; (save continue)
;; (save n)
;; (debug)
;; ;Value: execution-halted
;; ------------------
;; (set-register-contents! factorial-machine 'n 8)
;; ;; 1 ]=>
;; ;Value: done
;; (proceed-machine factorial-machine)
;; ---OUTPUT---
;; 1 ]=>
;; (assign n (op -) (reg n) (const 1))
;; (assign continue (label after-fact))
;; (goto (label fact-loop))
;; fact-loop
;; (test (op =) (reg n) (const 1))
;; (branch (label base-case))
;; (save continue)
;; (save n)
;; (assign n (op -) (reg n) (const 1))
;; (assign continue (label after-fact))
;; (goto (label fact-loop))
;; fact-loop
;; (test (op =) (reg n) (const 1))
;; (branch (label base-case))
;; (save continue)
;; (save n)
;; ....
;; after-fact
;; (restore n)
;; (restore continue)
;; (assign val (op *) (reg n) (reg val))
;; (goto (reg continue))
;; after-fact
;; (restore n)
;; (restore continue)
;; (assign val (op *) (reg n) (reg val))
;; (goto (reg continue))
;; after-fact
;; ...
;; (assign val (op *) (reg n) (reg val))
;; (goto (reg continue))
;; fact-done
;; ;Value: done
;; (get-register-contents factorial-machine 'val)
;; 1 ]=>
;; ;Value: 20160

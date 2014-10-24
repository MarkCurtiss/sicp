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
;; (load "book_code/ch5-regsim.scm")

;; (define (make-factorial-machine)
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

;; (define factorial-machine (make-factorial-machine))

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

;; ;; There is a BUG in this implementation!  If the breakpoint is set after any
;; ;; (goto) or (branch) statement it gets erased.  I think this is because of
;; ;; of the modification of the instruction sequence that happens in gotos and
;; ;; branches.

;; (define factorial-machine (make-factorial-machine))
;; (set-register-contents! factorial-machine 'n 4)
;; (set-breakpoint factorial-machine 'fact-loop 5)
;; (enable-instruction-tracing factorial-machine)
;; (cancel-all-breakpoints factorial-machine)
;; (start factorial-machine)
;; ;;
;; ;; (perform (op initialize-stack))
;; ;; (assign continue (label fact-done))
;; ;; fact-loop
;; ;; (test (op =) (reg n) (const 1))
;; ;; ...
;; ;; etc etc.

;; (load "book_code/ch5-regsim.scm")
;; (define factorial-machine (make-factorial-machine))
;; (set-register-contents! factorial-machine 'n 4)
;; (set-breakpoint factorial-machine 'fact-loop 5)
;; (enable-instruction-tracing factorial-machine)
;; (cancel-breakpoint factorial-machine 'fact-loop 5)
;; 1 ]=>
;; ;Value: breakpoint-set
;; (start factorial-machine)

;; 1 ]=>
;; (perform (op initialize-stack))
;; (assign continue (label fact-done))
;; fact-loop
;; (test (op =) (reg n) (const 1))
;; (branch (label base-case))
;; (save continue)
;; (save n)
;; (assign n (op -) (reg n) (const 1))
;;execution proceeds normally

; 5.20
; ========================================================================
;; I drew a box and pointer diagram on my notebook.  I guess I will bring it
;; to class next week.

;; Here is the memory-vector representation.
;;            1  | 2  | 3  | 4  | 5
;; the-cars | p2 | n1 | p2 | n2 |
;; the-cdrs | p3 | p4 | e0 | e0 |

;; The free pointer will be pointed at p5.

; 5.21
; ========================================================================
;; a. See the test "it can implement a recursive tree-leaf counter"

;; b.
;; This *should* work, but due to incompatibility issues with multi-stack-register-machine
;; and book_code/ch5-regsim.scm, it fails to run.
;; (load "multi-stack-register-machine.scm")

;; (define (not-pair? x)
;;   (not (pair? x)))

;; (define iterative-leaf-counter
;;   (make-machine
;;    '(continue tree num-leaves)
;;    (list
;;     (list '+ +)
;;     (list 'null? null?)
;;     (list 'not-pair? not-pair?)
;;     (list 'car car)
;;     (list 'cdr cdr)
;;     )
;;    '(
;;      (perform (op initialize-stack))
;;      (assign continue (label count-done))
;;      (assign num-leaves (const 0))

;;      count-loop
;;       (test (op null?) (reg tree))
;;       (branch (label null))
;;       (test (op not-pair?) (reg tree))
;;       (branch (label not-pair))
;;       (save continue)
;;       (assign continue (label after-car))
;;       (save tree)
;;       (assign tree (op car) (reg tree))
;;       (goto (label count-loop))

;;      after-car
;;       (restore continue)
;;       (restore tree)
;;       (assign tree (op cdr) (reg tree))
;;       (goto (label count-loop))

;;      not-pair
;;       (assign num-leaves (op +) (reg num-leaves) (const 1))
;;       (restore continue)
;;       (goto (reg continue))

;;      null
;;       (goto (reg continue))
;;       count-done)
;; ))

;; (define tree (cons (cons 1 2) (cons 3 4)))
;; (set-register-contents! iterative-leaf-counter 'tree tree)

;; (start iterative-leaf-counter)
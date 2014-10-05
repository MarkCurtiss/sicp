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

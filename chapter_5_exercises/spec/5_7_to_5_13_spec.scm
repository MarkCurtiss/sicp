(load "5_7_to_5_13.scm")

(describe "Register machine simulator"
  (it "can simulate a recursive exponentiation machine"
    (lambda ()
      (load "book_code/ch5-regsim.scm")

      (define expt-machine
	(make-machine
	 '(b n continue val)
	 (list
	  (list '= =)
	  (list '- -)
	  (list '* *))
	 '((assign continue (label expt-done))
	   expt-loop
	   (test (op =) (reg n) (const 0))
	   (branch (label base-case))
	   (save continue)
	   (save n)
	   (assign n (op -) (reg n) (const 1))
	   (assign continue (label after-expt))
	   (goto (label expt-loop))
	   after-expt
	   (restore n)
	   (restore continue)
	   (assign val (op *) (reg b) (reg val))
	   (goto (reg continue))
	   base-case
	   (assign val (const 1))
	   (goto (reg continue))
	   expt-done)
	 ))

      (set-register-contents! expt-machine 'b 9)
      (set-register-contents! expt-machine 'n 4)
      (start expt-machine)

      (assert
       (=
	(get-register-contents expt-machine 'val)
	6561))
      ))

  (it "can simulate an iterative exponentiation machine"
    (lambda ()
      (load "book_code/ch5-regsim.scm")

      (define expt-machine
	(make-machine
	 '(b n product)
	 (list
	  (list '= =)
	  (list '- -)
	  (list '* *))
	 '((assign product (const 1))
	  test-counter
	  (test (op =) (reg n) (const 0))
	  (branch (label expt-done))
	  (assign n (op -) (reg n) (const 1))
	  (assign product (op *) (reg b) (reg product))
	  (goto (label test-counter))
	  expt-done)))

      (set-register-contents! expt-machine 'b 9)
      (set-register-contents! expt-machine 'n 4)
      (start expt-machine)

      (assert
       (=
	(get-register-contents expt-machine 'product)
	6561))
      ))

  (it "throws an error if you have duplicate label names"
    (lambda ()
      (load "book_code/ch5-regsim.scm")

      (define (make-ambiguous-machine)
	(make-machine
	 '(a)
	 (list '())
	 '(
	   start
	   (goto (label here))
	   here
	   (assign a (const 3))
	   (goto (label there))
	   here
	   (assign a (const 4))
	   (goto (label there))
	   there
	   ))
	)

      (assert-error
       (lambda () (make-ambiguous-machine))
       "Redundant label detected: here")
      ))

  (it "can't apply operations to labels"
    (lambda ()
      (load "book_code/ch5-regsim.scm")

      (define (make-invalid-machine)
	(make-machine
	 '(a)
	 (list (list '* *))
	 '(
	   a-label
	   (assign a (const 3))
	   (assign a (op *) (reg a) (label a-label))
	   done))
	)

      (assert-error
       (lambda () (make-invalid-machine))
       "Attempted to apply an operation to a label -- MAKE-OPERATION-EXP")
      ))
  )

(describe "Alternate syntax register machine simulator"
  (it "uses alternative syntax"
    (lambda ()
      (load "alternate-syntax-regsim.scm")

      (define expt-machine
	(make-machine
	 '(b n product)
	 (list
	  (list '= =)
	  (list '- -)
	  (list '* *))
	 '((assign product (const 1))
	  test-counter
	  (test (operator =) (register n) (const 0))
	  (jump-if-true (label expt-done))
	  (assign n (operator -) (register n) (const 1))
	  (assign product (operator *) (register b) (register product))
	  (goto (label test-counter))
	  expt-done)))

      (set-register-contents! expt-machine 'b 9)
      (set-register-contents! expt-machine 'n 4)
      (start expt-machine)

      (assert
       (=
	(get-register-contents expt-machine 'product)
	6561))
      ))
  )

(describe "named-stack register machine simulator"
  (it "errors out if you try and restore a stack value to the wrong register"
    (lambda ()
      (load "named-stack-regsim.scm")

      (define invalid-machine
	(make-machine
	 '(x y)
	 (list ())
	 '(
	   a-label
	   (assign x (const 4))
	   (assign y (const 8))
	   (save x)
	   (save y)
	   (restore x)
	   done))
	)

      (assert-error
       (lambda () (start invalid-machine))
       "Attempted to restore the wrong stack value to a register: x"))
    )
  )

(describe "multi-stack register machine simulator"
  (it "associates each register with its own stack"
    (lambda ()
      (load "multi-stack-register-machine.scm")

      (define stacked-machine
	(make-machine
	 '(x y)
	 (list ())
	 '(
	  start
	   (perform (op initialize-stack))
	   (assign x (const 4))
	   (assign y (const 8))

	   (save x)
	   (save y)

	   (assign x (const 16))
	   (assign y (const 64))

	   (restore x)
	   (restore y)
	  done))
	)

      (start stacked-machine)

      (assert
       (=
	(get-register-contents stacked-machine 'x)
	4))

      (assert
       (=
	(get-register-contents stacked-machine 'y)
	8))
      ))
  )

(describe "reflective register machine"
  (it "keeps track of every instruction"
    (lambda ()
      (load "reflective-register-machine.scm")

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

      (assert (equal?
	       '(assign branch goto restore save test)
	       (instruction-set fib-machine)
	       ))
      ))

  (it "keeps track of every register that was the target of a (goto)"
    (lambda ()
      (load "reflective-register-machine.scm")

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

      (assert (equal?
	       '(continue)
	       (goto-register-set fib-machine)
	       ))
      ))

  (it "keeps track of every register that was the target of a (save) or (restore)"
    (lambda ()
      (load "reflective-register-machine.scm")

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

      (assert (equal?
	       '(continue n val)
	       (value-register-set fib-machine)
	       ))
      ))
  )


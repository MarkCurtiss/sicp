(load "5_14_to_5_22.scm")

(describe "Register machine simulator"
  (it "counts the number of stack pushes to compute N!"
    (lambda ()
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
  )

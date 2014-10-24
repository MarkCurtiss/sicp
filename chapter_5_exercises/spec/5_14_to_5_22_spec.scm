(load "5_14_to_5_22.scm")

(describe "Register machine simulator"
  (it "tracks the number of instructions executed"
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
	   )
	 ))

      (set-register-contents! factorial-machine 'n 1)
      (start factorial-machine)
      (assert (= (get-instruction-count factorial-machine) 6))

      (set-register-contents! factorial-machine 'n 6)
      (start factorial-machine)
      (assert (= (get-instruction-count factorial-machine) 61))
      ))

  (it "can implement a recursive tree-leaf counter"
    (lambda ()
      (load "book_code/ch5-regsim.scm")

      (define (not-pair? x)
	(not (pair? x)))

      (define recursive-leaf-counter
	(make-machine
	 '(continue tree num-leaves)
	 (list
	  (list '+ +)
	  (list 'null? null?)
	  (list 'not-pair? not-pair?)
	  (list 'car car)
	  (list 'cdr cdr)
	  )
	 '((perform (op initialize-stack))
	   (assign continue (label count-done))
	  count-loop
	   (test (op null?) (reg tree))
	   (branch (label null-case))
	   (test (op not-pair?) (reg tree))
	   (branch (label not-pair))
	   (save continue)
	   (assign continue (label after-car))
	   (save tree)
	   (assign tree (op car) (reg tree))
	   (goto (label count-loop))
	  after-car
	   (restore tree)
	   (assign tree (op cdr) (reg tree))
	   (assign continue (label after-cdr))
	   (save num-leaves)
	   (goto (label count-loop))
	  after-cdr
	   (assign tree (reg num-leaves))
	   (restore num-leaves)
	   (restore continue)
	   (assign num-leaves (op +) (reg tree) (reg num-leaves))
	   (goto (reg continue))
	  null-case
	   (assign num-leaves (const 0))
	   (goto (reg continue))
	  not-pair
	   (assign num-leaves (const 1))
	   (goto (reg continue))
	   count-done)
	 ))

      (define tree (cons (cons 1 2) (cons 3 4)))
      (set-register-contents! recursive-leaf-counter 'tree tree)
      (start recursive-leaf-counter)
      (assert (equal?
	       (get-register-contents recursive-leaf-counter 'num-leaves)
	       4))
      ))

  (it "prints out every instruction that was executed"
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
	   ))
	)

      (set-register-contents! factorial-machine 'n 2)
      (enable-instruction-tracing factorial-machine)

      (assert
       (equal?
	(with-output-to-string
	  (lambda  () (start factorial-machine)))
	"
(perform (op initialize-stack))
(assign continue (label fact-done))
fact-loop
(test (op =) (reg n) (const 1))
(branch (label base-case))
(save continue)
(save n)
(assign n (op -) (reg n) (const 1))
(assign continue (label after-fact))
(goto (label fact-loop))
fact-loop
(test (op =) (reg n) (const 1))
(branch (label base-case))
base-case
(assign val (const 1))
(goto (reg continue))
after-fact
(restore n)
(restore continue)
(assign val (op *) (reg n) (reg val))
(goto (reg continue))
fact-done"
       ))
     ))

  (it "can trace register assignments"
    (lambda ()
      (load "book_code/ch5-regsim.scm")

      (define register-machine
	(make-machine
	 '(traced untraced)
	 '()
	 '(
	   a-label
	   (assign traced (const 3))
	   (assign untraced (const 5))

	   (assign traced (const 8))
	   )))

      (enable-register-tracing (get-register register-machine 'traced))

      (assert
       (equal?
	(with-output-to-string
	  (lambda () (start register-machine)))
	"
(Register traced is being assigned 3 from the previous value: *unassigned*)
(Register traced is being assigned 8 from the previous value: 3)"
	  ))


      ))

  )

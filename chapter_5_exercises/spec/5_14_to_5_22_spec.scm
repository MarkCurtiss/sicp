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

  (it "can (append) two lists together"
    (lambda ()
      (load "book_code/ch5-regsim.scm")

      (define append-machine
	(make-machine
	 '(head list-1 list-2 result-list)
	 (list
	  (list 'car car)
	  (list 'cdr cdr)
	  (list 'null? null?)
	  (list 'cons cons)
	  (list 'reverse reverse)
	  )
	 '((perform (op initialize-stack))
	   (assign result-list (const ()))
	   (assign head (op reverse) (reg list-1))   ; so we can build a proper
	   (assign list-1 (op reverse) (reg list-2)) ; list by cons'ing onto
	   (assign list-2 (reg head))		     ; the head and ending with '()
	  copy-list-1
	   (test (op null?) (reg list-1))
	   (branch (label copy-list-2))
	   (assign head (op car) (reg list-1))
	   (test (op null?) (reg result-list))
	   (branch (label start-result-list))
	   (assign result-list
		   (op cons) (reg head) (reg result-list))
	   (assign list-1 (op cdr) (reg list-1))
	   (goto (label copy-list-1))
	  copy-list-2
	   (test (op null?) (reg list-2))
	   (branch (label done))
	   (assign head (op car) (reg list-2))
	   (assign result-list
		   (op cons) (reg head) (reg result-list))
	   (assign list-2 (op cdr) (reg list-2))
	   (goto (label copy-list-2))
	  start-result-list
	   (assign result-list
		   (op cons) (reg head) (const ()))
	   (assign list-1 (op cdr) (reg list-1))
	   (goto (label copy-list-1))
	  done)
	 ))

      (define x '(1 2 3))
      (define y '(4 5))

      (set-register-contents! append-machine 'list-1 x)
      (set-register-contents! append-machine 'list-2 y)

      (start append-machine)

      (assert (equal?
	       (get-register-contents append-machine 'result-list)
	       (list 1 2 3 4 5)))
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
          fact-done))
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

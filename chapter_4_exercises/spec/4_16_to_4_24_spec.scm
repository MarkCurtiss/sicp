(load "4_16_to_4_24.scm")

(describe "lookup-variable-value"
  (it "throws an error if it finds the symbol *unassigned*"
    (lambda ()
      (define env (extend-environment '(oceansize) '(*unassigned*) the-empty-environment))

      (assert-error
       (lambda () (lookup-variable-value 'oceansize env))
	"Unassigned variable -- LOOKUP-VARIABLE-VALUE oceansize")
      ))
  )

(describe "scan-out-defines"
  (it "transforms defines into lets"
    (lambda ()
      (define exp '(lambda (x)
		     (define u 3)
		     (define v 4)
		     (* 3 4)))

      (define transformed-exp '(lambda (x)
				 (let ((u '*unassigned*) (v '*unassigned*))
				   (set! u 3)
				   (set! v 4)
				   (* 3 4))))

      (assert
       (equal?
	(scan-out-defines exp)
	transformed-exp))
      ))
  )

(describe "letrec"
  (it "translates into nested lets that allow mutual recursion"
    (lambda ()
      (define exp '(letrec ((fact
			      (lambda (n)
				(if (= n 1)
				    1
				    (* n (fact (- n 1)))))))
		     (fact 10)))

      (define transformed-exp '(let ((fact '*unassigned*))
				 (let ((inner-fact (lambda (n)
					    (if (= n 1)
						1
						(* n (fact (- n 1)))))))
				   (set! fact inner-fact))
				 (fact 10)))

      (assert
       (equal?
	(transform exp)
	transformed-exp))
      ))
  )




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

(describe "define-less recursion"
  (it "computes factorial without defining functions"
      (lambda ()
	(assert (=
		 (
		   (lambda (n)
		    (
		     (lambda (fact)
		       (fact fact n))
		     (lambda (ft k)
		       (if (= k 1)
			   1
			   (* k (ft ft (- k 1)))))
		     )
		    )
		  10)
		 (* 10 9 8 7 6 5 4 3 2 1)
		 ))
	))

  (it "computes fibonacci without defining functions"
    (lambda ()
      (assert (=
	       ((lambda (n)
		  (
		   (lambda (fib)
		     (fib fib n))
		   (lambda (ft k)
		     (cond ((= k 0) 0)
			   ((or (= k 1) (= k 2)) 1)
			   (else (+ (ft ft (- k 1)) (ft ft (- k 2))))))
		   )
		  )
		8)
	       21)
	      ))
    )

  (it "determines odd/even without defining functions"
    (lambda ()
      (define (f x)
	((lambda (even? odd?)
	   (even? even? odd? x))
	 (lambda (ev? od? n)
	   (if (= n 0) true (od? ev? od? (- n 1))))
	 (lambda (ev? od? n)
	   (if (= n 0) false (ev? ev? od? (- n 1))))))

      (assert (eq? (f 7) #f))
    ))
  )



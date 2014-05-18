(load "4_1_to_4_10.scm")

(describe "List of values"
  (it "evaluates from left to right"
    (lambda ()
      (define env '())

      (assert
       (equal?
	(list-of-values-left-to-right '(1 2 () 3 4) user-initial-environment)
	'(1 2 () 3 4)))
      ))

  (it "evaluates from right to left"
    (lambda ()
      (assert
       (equal?
	(list-of-values-right-to-left '(1 2 () 3 4) user-initial-environment)
	'(((((() . 4) . 3)) . 2) . 1)))
       ))
  )

(describe "(louis-application?)"
  (it "only considers expressions starting with (call) as procedure applications"
    (lambda ()
      (assert
       (false?
	(louis-application? '(factorial 3))))

      (assert
       (false?
	(louis-application? '(+ 1 2))))

      (assert
       (eq? true
	(louis-application? '(call factorial 3))))

      (assert
       (eq? true
	(louis-application? '(call + 1 2))))
      ))
  )

(describe "eval"
  (it "evaluates numbers and strings"
    (lambda ()
      (define env '())

      (assert
       (= 3
	  (eval 3 env)))
      ))

  (it "evaluates if statements"
    (lambda ()
      (define env '())

      (assert
       (= 4
	  (eval '(if 3 4 5) env)))
      ))

  (it "evaluates booleans"
    (lambda ()
      (define env '())

      (assert
       (false?
	(eval false env)))

      (assert
       (true?
	(eval true env)))
      ))

  (it "understands the empty list"
    (lambda ()
      (define env '())

      (assert
       (eq? '()
	  (eval '() env)))
      ))

  (it "evaluates (and) statements"
    (lambda ()
      (define env '())

      (assert
       (false?
	(eval '(and #f #t) env)))

      (assert
       (true?
	(eval '(and #t 1) env)))
      ))

  (it "evalutes (or) statements"
    (lambda ()
      (define env user-initial-environment)

      (assert
       (false?
	(eval '(or #f #f) env)))

      (assert
       (true?
	(eval '(or #f #t 1) env)))
      ))

  (it "handles standard (cond) syntax"
    (lambda ()
      (define exp '(cond ((> x 0) x)
			((= x 0) (display zero) 0)
			(else (- x))))

      (assert
       (equal?
       (transform exp)
       (list 'if '(> x 0) 'x '(if (= x 0) (begin (display zero) 0) (- x)))))
      ))

  (it "transforms (cond) (test => recipient) into standard cond syntax"
    (lambda ()
      (define exp '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
			(else #f)))

      (define transformed-exp '(if (assoc 'b '((a 1) (b 2)))
				   (cadr (assoc 'b '((a 1) (b 2))))
				   #f))

      (assert
       (equal?
	(transform exp)
	transformed-exp
	))
      ))

  (it "transforms (let) expressions into (lambdas)"
    (lambda ()
      (define exp '(let ((x 1) (y 2))
		     (cons x y)))

      (define transformed-exp '((lambda (x y)
				  (cons x y))
				1 2))
      (assert
       (equal?
	(transform exp)
	transformed-exp))
      ))

  (it "handles lambda expressions"
    (lambda ()
      (define env 'test-env)
      (define exp '(lambda (x y)
		     (cons x y)))

      (define transformed-exp '(procedure (x y) (cons x y) test-env))

      (assert
       (equal?
	(eval exp env)
	transformed-exp))
      ))

  (it "transforms let* into nested lets"
    (lambda ()
      (define exp '(let* ((x 3)
			  (y (+ x 2))
			  (z (+ x y 5)))
		     (* x z)))

      (define transformed-exp '(let ((x 3))
				 (let ((y (+ x 2)))
				   (let ((z (+ x y 5)))
				     (* x z)))))

      (assert
       (equal?
	(transform exp)
	transformed-exp))
      ))

  (it "lets you create variable definitions"
    (lambda ()
      (assert
       (equal?
	(make-define 'x 3)
	'(define x 3)))
      ))

  (it "lets you create function definitions"
    (lambda ()
      (assert
       (equal?
	(make-define '(owls the lion) '(pp "ate its trainer at the circus"))
	'(define (owls the lion)
	   (pp "ate its trainer at the circus"))))
      ))

  (it "transforms named (let)s"
    (lambda ()
      (define exp '(let fib-iter ((a 1)
				  (b 0)
				  (count n))
		     (if (= count 0)
			 b
			 (fib-iter (+ a b) a (- count 1)))))

      (define transformed-exp '(begin
				 (define (fib-iter a b count)
				   (if (= count 0)
				       b
				       (fib-iter (+ a b) a (- count 1))))
				 (fib-iter (1 0 n))))

      (assert
       (equal?
	(transform exp)
	transformed-exp))
      ))

  (it "has other looping constructs like (while)"
    (lambda ()
      (define exp '(while (< lines-in-file 100)
		     (read-next-line-in-file)
		     (+ lines-in-file 1)
		     ))

      (define transformed-exp '(define (iter)
				 (if (< lines-in-file 100)
				     (begin
				       (read-next-line-in-file)
				       (+ lines-in-file 1)
				       (iter))
				     #f)))

      (assert
       (equal?
	(transform exp)
	transformed-exp))
      ))
  )

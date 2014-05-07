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
      (define env user-initial-environment)

      (define exp '(cond ((> x 0) x)
			((= x 0) (display zero) 0)
			(else (- x))))

      (assert
       (equal?
       (transform exp)
       (list 'if '(> x 0) 'x '(if (= x 0) (begin (display zero) 0) (- x)))))
      ))
)

(load "2_77_to_2_97.scm")

(describe "Polynomials"
  (it "lets you add dense-term-list polynomials" (lambda ()
    (assert (equal?
      (add (make-polynomial 'x (make-dense-term-list '(2 0 0 0 0 0 0 0 3)))
	   (make-polynomial 'x (make-dense-term-list '(6 0 0 0 0 0 0 0 0 0 0 0 0 0 8))))
      (make-polynomial 'x (make-dense-term-list '(6 0 0 0 0 0 2 0 0 0 0 0 0 0 11)))))))

  (it "lets you multiply dense-term-list polynomials" (lambda ()
    (assert (equal?
      (mul (make-polynomial 'x (make-dense-term-list '(2 0 0 0 0 0 0 0 3)))
	   (make-polynomial 'x (make-dense-term-list '(6 0 0 0 0 0 0 0 0 0 0 0 0 0 8))))
      (make-polynomial 'x (make-dense-term-list '(12 0 0 0 0 0 0 0 18 0 0 0 0 0 16 0 0 0 0 0 0 0 24)))))))

  (it "lets you subtract dense-term-list polynomials" (lambda ()
    (assert (equal?
      (sub (make-polynomial 'x (make-dense-term-list '(2 0 0 0 0 0 0 0 3)))
	   (make-polynomial 'x (make-dense-term-list '(6 0 0 0 0 0 0 0 0 0 0 0 0 0 8))))
      (make-polynomial 'x (make-dense-term-list '(-6 0 0 0 0 0 2 0 0 0 0 0 0 0 -5)))))))

  (it "lets you divide dense-term-list polynomials" (lambda ()
    (assert (equal?
      (div (make-polynomial 'x (make-dense-term-list '(1 0 0 0 0 -1)))
	   (make-polynomial 'x (make-dense-term-list '(1 0 -1))))
      (list 'polynomial 'x '(dense-term-list 1 0 1 0) '(dense-term-list 1 -1))))))

  (it "lets you add sparse-term-list polynomials" (lambda ()
    (assert (equal?
      (add (make-polynomial 'x (make-sparse-term-list (list '(0 3) '(8 2))))
	   (make-polynomial 'x (make-sparse-term-list (list '(0 8) '(14 6)))))
      (make-polynomial 'x (make-sparse-term-list (list (list 0 (cons 'scheme-number 11)) '(14 6) '(8 2))))))))

  (it "lets you (drop) polynomials down a variable"
      (lambda ()
	;;(drop y) == (0x + y)
	(assert (equal?
		 (drop (make-polynomial 'y (make-sparse-term-list (list '(1 1)))))
		 (make-polynomial 'x (make-sparse-term-list (list (list 0
								  (make-polynomial 'y (make-sparse-term-list (list '(1 1))))))))))))
  (it "lets you add x and y together"
      (lambda ()
	(define x-poly (make-polynomial 'x (make-sparse-term-list (list '(1 1)))))
	(define y-poly (make-polynomial 'y (make-sparse-term-list (list '(1 1)))))

	;;x + y
	(assert (equal?
		 (add x-poly y-poly)
		 (make-polynomial 'x
				  (make-sparse-term-list
				   (list (list 1 1)
					 (list 0
					       (make-polynomial 'y (make-sparse-term-list (list '(1 1))))))))))))

  (it "can compute the greatest common divisor for two polynomials"
      (lambda ()
	(define p1 (make-polynomial 'x (make-sparse-term-list (list '(4 1) '(3 -1) '(2 -2) '(1 2)))))
	(define p2 (make-polynomial 'x (make-sparse-term-list (list '(3 1) '(1 -1)))))

	(assert (equal?
		 (greatest-common-divisor p1 p2)
		 (list 'polynomial 'sparse-term-list '(2 (scheme-number . -1)) '(1 (scheme-number . 1)))))))

  (it "computes polynomial greatest common divisor with reduced integer coefficients"
      (lambda ()
	(define p1 (make-polynomial 'x (make-dense-term-list '(1 -2 1))))
	(define p2 (make-polynomial 'x (make-dense-term-list '(11 0 7))))
	(define p3 (make-polynomial 'x (make-dense-term-list '(13 5))))
	(define q1 (mul p1 p2))
	(define q2 (mul p1 p3))

	(assert (equal?
		 (greatest-common-divisor q1 q2)
		 (list 'polynomial 'dense-term-list 1 -2 1)))))

  (it "lets you reduce-terms on two term-lists"
      (lambda ()
	(define n (make-dense-term-list '(18 3 0 6)))
	(define d (make-dense-term-list '(9)))

	(assert (equal?
		 (reduce-terms n d)
		 (list
		  (make-dense-term-list '(6 1 0 2))
		  (make-dense-term-list '(3)))))))

  (it "lets you reduce-poly on two polynomials"
      (lambda ()
	(define n (make-polynomial 'x (make-dense-term-list '(18 3 0 6))))
	(define d (make-polynomial 'x (make-dense-term-list '(9))))

	(assert (equal?
		 (reduce-poly n d)
		 (list
		  (make-polynomial 'x (make-dense-term-list '(6 1 0 2)))
		  (make-polynomial 'x (make-dense-term-list '(3))))))))

  (it "adds rational numbers together"
      (lambda ()
	(assert (equal?
		 (add (make-rational 8 1) (make-rational 14 3))
		 (make-rational 38 3)))))


  (it "reduces rational polynomials to lowest common terms"
      (lambda ()
	(define p1 (make-polynomial 'x (make-dense-term-list '(1 1))))
	(define p2 (make-polynomial 'x (make-dense-term-list '(1 0 0 -1))))
	(define p3 (make-polynomial 'x (make-dense-term-list '(1 0))))
	(define p4 (make-polynomial 'x (make-dense-term-list '(1 0 -1))))

	(define rf1 (make-rational p1 p2))
	(define rf2 (make-rational p3 p4))

	(add rf1 rf1)))

  (it "doesn't try to (drop) data types with no (project) method"
      (lambda ()
	(define fake-type (attach-tag 'fake-type 800))
	(put 'add '(fake-type fake-type) (lambda (x y) (+ x y)))

	(assert (equal?
		 (+ 800 800)
		 (add fake-type fake-type)))))
)
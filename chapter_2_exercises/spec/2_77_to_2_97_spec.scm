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

  (it "lets you create polynomials in multiple variables" (lambda ()
    (assert (equal?
      ;;xy^2 - xy
      (make-polynomial 'x (make-dense-term-list (list (make-polynomial 'y (make-dense-term-list '(1 0 -1))))))
      (list 'polynomial 'x 'dense-term-list '(polynomial y dense-term-list 1 0 -1))))))

  (it "shouldn't reduce rationals of polynomials to lowest terms"
      (lambda ()
	(define p1 (make-polynomial 'x (make-sparse-term-list (list '(2 1) '(0 1)))))
	(define p2 (make-polynomial 'x (make-sparse-term-list (list '(3 1) '(0 1)))))
	(define rf (make-rational p2 p1))

	(assert (equal?
		 (add rf rf)
		 (list 'rational '(polynomial x sparse-term-list ((2 1)(0 1))) '(polynomial x sparse-term-list ((3 1)(0 1))))))))

  (it "can compute the greatest common divisor for two polynomials"
      (lambda ()
	(define p1 (make-polynomial 'x (make-sparse-term-list (list '(4 1) '(3 -1) '(2 -2) '(1 2)))))
	(define p2 (make-polynomial 'x (make-sparse-term-list (list '(3 1) '(1 -1)))))

	(assert (equal?
		 (greatest-common-divisor p1 p2)
		 (list 'polynomial 'sparse-term-list '(2 (scheme-number . -1)) '(1 (scheme-number . 1)))))))

)
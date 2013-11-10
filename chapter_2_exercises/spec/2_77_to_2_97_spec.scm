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
      (make-polynomial 'x (make-polynomial 'y (make-dense-term-list '(1 0 -1))))
      (list 'polynomial 'x 'polynomial 'y 'dense-term-list 1 0 -1)))))



)
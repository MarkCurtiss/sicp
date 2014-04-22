(load "3_77_to_3_82.scm")

(describe "Delayed integral"
  (it "can be used in solvers with delayed functions"
    (lambda ()
      (define (solve f y0 dt)
	(define y (integral (delay dy) y0 dt))
	(define dy (stream-map f y))
	y)

      (assert (equal?
	       (stream-ref (solve (lambda (y) y) 1 0.001) 1000)
	       2.716923932235896))
      ))

  (it "can solve 2nd order differential equations"
    (lambda ()

      (define 2nd-deriv-test (solve-2nd 2 3 5 8 13))

      (assert (equal?
	       (first-n-elements-of-stream 2nd-deriv-test 5)
	       '(8 73 1388 21328 344768)))

      ))

  (it "can solve some other type of 2nd order differential equations"
    (lambda ()
      (define f (lamdba (x y) (+ (square x) (cube y))))

      (define 2nd-deriv-test (solve-2nd-f 2 3 5 8 13 f))
  )

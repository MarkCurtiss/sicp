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
  )

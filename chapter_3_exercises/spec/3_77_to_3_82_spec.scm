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
      (define 2nd-deriv-test (solve-2nd-with-f * 3 5 8))

      (assert (equal?
	       (first-n-elements-of-stream 2nd-deriv-test 5)
	       '(5 29 413 34205 41936285)))
      ))
  )

(describe "RLC circuits"
  (it "takes a bunch of inputs and outputs a stream vC (voltage across the capacitor) and iL (current in the inductor)"
    (lambda ()
      (define vC0 10)
      (define iL0 0)
      (define RLC1 (RLC 1 1 0.2 0.1))

      (define output-streams (RLC1 vC0 il0))

      (assert (equal?
	       (first-n-elements-of-stream (car output-streams) 5)
	       '(10 10 9.5 8.55 7.220000000000001)))

      (assert (equal?
	       (first-n-elements-of-stream (cdr output-streams) 5)
	       '(0 1. 1.9 2.66 3.249)))
      ))
  )

(describe "rand"
  (it "lets you generate random numbers from a stream of input requests"
    (lambda ()
      (define generate (list 'generate 'throwawayvalue))
      (define inputs (cons-stream generate inputs))

      (define rands (rand inputs))

      (assert (equal?
	       (first-n-elements-of-stream rands 3)
	       '(0 1 2)))
      ))

  (it "lets you reset the sequence"
    (lambda ()
      (define generate (list 'generate 'throwawayvalue))
      (define reset (list 'reset 8))

      (define inputs (stream generate generate reset generate generate generate))
      (define rands (rand inputs))

      (assert (equal?
	       (first-n-elements-of-stream rands 5)
	       '(0 1 2 8 9)))
    ))
  )
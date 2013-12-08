(load "3_1_to_3_8.scm")

(describe "accumulator"
  (it "accumulates a sum"
    (lambda ()
      (define A (make-accumulator 5))
      (assert (equal?
	       (A 10)
	       15))
      (assert (equal?
	       (A 10)
	       25)))))

(describe "make-monitored"
  (it "returns a function that you can call normally"
    (lambda ()
      (define s (make-monitored sqrt))

      (assert (equal?
	       (s 100)
	       10))))

  (it "tells you how many times it's been called if you pass a special symbol"
    (lambda ()
      (define s (make-monitored sqrt))

      (s 100)
      (assert (equal?
	       (s 'how-many-calls)
	       1))
      (s 100)
      (assert (equal?
	       (s 'how-many-calls)
	       2))))

  (it "lets you reset the call count"
    (lambda ()
      (define s (make-monitored sqrt))

      (s 100)
      (s 'reset-count)

      (assert (equal?
	       (s 'how-many-calls)
	       0)))))

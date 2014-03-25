(load "3_50_to_3_62.scm")

(describe "Factorial"
  (it "computes factorials"
    (lambda ()
      (assert (equal?
	       (stream-ref factorial 4)
	       24))))
)

(describe "Partial sums"
  (it "computes the partial sum of a sequence"
    (lambda ()
      (define partial-ints (partial-sums integers))

      (assert (equal?
	       (stream-ref partial-ints 0)
	       1))

      (assert (equal?
	       (stream-ref partial-ints 2)
	       6))

      (assert (equal?
	       (stream-ref partial-ints 4)
	       15))))
)

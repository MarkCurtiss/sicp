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

(describe "Hamming streams"
  (it "contains positive integers with no factors other than 2, 3, 5"
    (lambda ()
      (assert (equal?
	       (stream-ref S 0)
	       1))

      (assert (equal?
	       (stream-ref S 1)
	       2))

      (assert (equal?
	       (stream-ref S 7)
	       9))))
)

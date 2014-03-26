(load "3_50_to_3_62.scm")

(describe "Factorial"
  (it "computes factorials"
    (lambda ()
      (assert (equal?
	       (first-n-elements-of-stream factorial 4)
	       '(1 2 6 24)))))
)

(describe "Partial sums"
  (it "computes the partial sum of a sequence"
    (lambda ()
      (define partial-ints (partial-sums integers))

      (assert (equal?
	       (first-n-elements-of-stream partial-ints 5)
	       '(1 3 6 10 15)))))
)

(describe "Hamming streams"
  (it "contains positive integers with no factors other than 2, 3, 5"
    (lambda ()

      (assert (equal?
	      (first-n-elements-of-stream S 12)
	      '(1 2 3 4 5 6 8 9 10 12 15 16)))))
)

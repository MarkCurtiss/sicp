(load "3_63_to_3_76.scm")

(describe "stream-limit"
  (it "computes a stream up to a given tolerance"
    (lambda ()
      (define (sqrt x tolerance)
	(stream-limit (sqrt-stream x) tolerance))

      (assert (equal?
	       (sqrt 4 .00000001)
	       2.))))
  )

(describe "all-pairs"
  (it "produces a stream of all pairs"
    (lambda ()
      (define first-5-pairs (first-n-elements-of-stream
			      (all-pairs integers integers)
			      5))

      (assert (equal?
	       first-5-pairs
	       '((1 1) (1 2) (1 2) (1 3) (2 2))))))
  )

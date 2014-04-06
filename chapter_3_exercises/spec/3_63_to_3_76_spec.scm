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




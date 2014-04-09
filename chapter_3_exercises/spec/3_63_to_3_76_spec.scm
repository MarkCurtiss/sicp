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

(describe "triples"
  (it "produces a stream of triples"
    (lambda ()
      (assert (equal?
	       (stream-car (triples integers integers integers))
	       '(1 1 1))
		 )))

  (it "produces triples such that i <= j <= k"
      (lambda ()
	(define (valid-triple? triple)
	  (let ((i (car triple))
		(j (cadr triple))
		(k (caddr triple)))
	     (<= i j k)))

	(define trips (triples integers integers integers))

	(assert (valid-triple?
		 (stream-car trips)))

	(assert (valid-triple?
		 (stream-ref trips 10)))
	))

  (it "lets you build pythagorean triples"
    (lambda ()
      (define (pythagorean? triple)
	(let ((i (car triple))
	      (j (cadr triple))
	      (k (caddr triple)))

	  (= (+ (square i) (square j))
	     (square k))))

      (define pythagorean-triples
	(stream-filter pythagorean?
		       (triples integers integers integers)))

      (assert (equal?
	       (stream-car pythagorean-triples)
	       '(3 4 5)))

      (assert (equal?
	       (stream-ref pythagorean-triples 1)
	       '(5 12 13)))
      ))
  )

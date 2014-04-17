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

(describe "merge-weighted"
  (it "merges two streams according to a weighting function"
    (lambda ()
      (define (penalize-ones x)
	(if (= x 1)
	   1
	   0))

      (define no-ones (merge-weighted ones (stream-cdr integers) penalize-ones))

      (assert (equal?
	       (first-n-elements-of-stream no-ones 6)
	       '(2 3 4 5 6 7)))
      ))
  )

(describe "weighted-pairs"
  (it "produces pairs of positive integers i <= j weighted by i+j"
    (lambda ()
      (define (weight pair)
	(+ (car pair) (cadr pair)))

      (define i-lt-j (weighted-pairs integers integers weight))

      (assert (equal?
	       (first-n-elements-of-stream i-lt-j 6)
	       '(
		 (1 1)
		 (1 2)
		 (1 3)
		 (2 2)
		 (1 4)
		 (2 3)
		 )))))

  (it "produces pairs of positive integers i <= j where i and j aren't divisible by 2, 3 or 5 and weighted by 2i + 3j + 5ij"
      (lambda ()
	(define (weight pair)
	  (let ((i (car pair))
		(j (cadr pair)))
	    (+ (* 2 i) (* 3 j) (* 5 i j))))

	(define (not-divisible-by-2-3-5? x)
	  (and
	   (not (= (remainder x 2) 0))
	   (not (= (remainder x 3) 0))
	   (not (= (remainder x 5) 0))))

	(define special-numbers (stream-filter not-divisible-by-2-3-5? integers))
	(define special-stream (weighted-pairs special-numbers special-numbers weight))

	(assert (equal?
		 (first-n-elements-of-stream special-stream 6)
		 '(
		   (1 1)
		   (1 7)
		   (1 11)
		   (1 13)
		   (1 17)
		   (1 19)
		   )
		 ))
      ))

  (it "finds Ramanujan numbeers"
    (lambda ()
      (assert (equal?
	       (ramanujan-numbers 5)
	       '(1729 4104 13832 20683 32832)
	       ))))

  (it "makes some other stupid random math stream that the authors find interesting"
    (lambda ()
      (assert (equal?
	       (some-other-stupid-random-math-stream-that-the-authors-find-interesting 2)
	       '((325 (1 18) (6 17) (10 15))
		 (425 (5 20) (8 19) (13 16)))
	       ))))
  )

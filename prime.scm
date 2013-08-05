(define (prime? n)
  (define (modified-expmod base exp m)
    (cond ((= exp 0) 1)
	  ((even? exp)
	   (let ((sqr (square (modified-expmod base (/ exp 2) m))))
	     (if (and
		  (not (= sqr 1))
		  (not (= sqr (- m 1)))
		  (= sqr (remainder 1 m)))
		 0
		 (remainder sqr m))
	     ))
	  (else
	   (remainder (* base (modified-expmod base (- exp 1) m))
		      m))))

  (define (miller-rabin-test n)
    (define (try-it a)
      (= (modified-expmod a (- n 1) n) 1))
    (try-it (+ 1 (random (- n 1)))))

  (define (miller-rabin-prime? n times)
    (cond ((= times 0) true)
	  ((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
	  (else false)))

  (miller-rabin-prime? n 10))






(load "3_1_to_3_8.scm")

(describe "make-accumulator"
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
(describe "make-account"
  (it "only processes calls if the password is correct"
    (lambda ()
      (define acc (make-account 100 'password))

      (assert (equal?
	       ((acc 'password 'withdraw) 40)
	       60))

      (pp ((acc 's~^Fu*t+GJ6Lmx6 'deposit) 50))

      (assert (equal?
	       ((acc 's~^Fu*t+GJ6Lmx6 'deposit) 50)
	       "Incorrect password"))
      ))

  (it "calls the cops if the password is wrong seven times"
    (lambda ()
      (define acc (make-account 100 'password))

      (define (call-acc-6-times)
	(define (iter current-value)
	  (cond ((< current-value 7)
		 ((acc 's~^Fu*t+GJ6Lmx6 'deposit) 50)
		 (iter (+ 1 current-value)))))
	(iter 0))

      (call-acc-6-times)

      (assert (equal?
	       ((acc 's~^Fu*t+GJ6Lmx6 'deposit) 50)
	       "Whee-oo whee-oo!  The cops have been called"))
      ))
  )

(describe "Monte Carlo integration"
  (it "generates random numbers in a range"
    (lambda ()
      (define random-number (random-in-range 3 18))
      (assert (equal? true
		      (and (>= random-number 3) (< random-number 18))))))

  (it "computes the area of a rectangle"
    (lambda ()
      (assert (equal?
	       (area-rect 2 4 8 12)
	       48))))

  (it "estimates the integral of a circle of radius 3 centered at (5,7)"
    (lambda ()

      (define (predicate x y)
	(<=
	 (+ (square (- x 5))
	    (square (- y 7)))
	 (square 3)))

      (assert (equal?
	       (floor (estimate-integral predicate 2.0 8.0 4.0 10.0 10000))
	       28.0))
      ))

  (it "estimates the integral of a unit circle"
    (lambda ()

      (define (predicate x y)
	(<= (+ (square x) (square y)) 1))

      (assert (equal?
	       (floor (estimate-integral predicate -1.0 1.0 -1.0 1.0 10000))
	       3.0))))
  )

(describe "rand"
  (it "lets you generate random numbers"
    (lambda ()
      (assert (equal?
	       (rand 'generate)
	       1))

      (rand 'generate)
      (rand 'generate)
      
      (assert (equal?
	       (rand 'generate)
	       4))
      ))

  (it "lets you reset the sequence"
    (lambda ()
      (rand 'generate)
      (rand 'generate)
      (rand 'generate)

      ((rand 'reset) 1)

      (assert (equal?
	       (rand 'generate)
	       2))
    ))
  )
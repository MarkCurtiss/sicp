; 1.9
; ========================================================================
; (define (inc a)
;   (+ 1 a))
;
; (define (dec a)
;   (- 1 a))
;
; (define (+ a b)
;   (if (= a 0)
;       b
;       (inc (+ (dec a) b))))
; When called with (+ 4 5), this expands to:
; (+ 4 5)
; (inc (+ (dec a) b))
; (inc (+ (dec 4) 5))
; (inc (+ 3 5))
; (inc (inc (+ (dec 3) 5)))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ (dec 2) 5))))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ (dec 1) 5)))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; (9)
; Which is recursive.

; (define (+ a b)
;   (if (= a 0)
;       b
;       (+ (dec a) (inc b))))
; When called with (+ 4 5), this expands to:
; (+ 4 5)
; (+ (dec a) (inc b))
; (+ (dec 4) (inc 5))
; (+ 3 6)
; (+ (dec 3) (inc 6))
; (+ 2 7)
; (+ (dec 2) (inc 7))
; (+ 1 8)
; (+ (dec 1) (inc 8))
; (+ 0 9)
; (9)
; which is iterative.

; 1.10
; ========================================================================
(define (Ackermann x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (Ackermann (- x 1)
			 (Ackermann x (- y 1))))))

;1 ]=> (Ackermann 1 10)
;Value: 1024

;1 ]=> (Ackermann 2 4)
;Value: 65536

;1 ]=> (Ackermann 3 3)
;Value: 65536

(define (f n) (Ackermann 0 n))
; f(n) = 2*n

(define (g n) (Ackermann 1 n))
; g(n) = 2^n

(define (h n) (Ackermann 2 n))
; h(n) =
; 2 when h = 0
; 2 ^ (h(n - 1)) otherwise

(define (k n) ( * 5 n n))
; k(n) = 5(n^2)

; 1.11
; ========================================================================

(define (f-recursive n)
  (if (< n 3)
       n
       (+
	(f-recursive (- n 1))
	(* 2 (f-recursive (- n 2)))
	(* 3 (f-recursive (- n 3))))))

(define (f-iterative n)
  (define (f-iter a b c n)
    (if (< n 3)
        a
        (f-iter (+ a (* 2 b) (* 3 c)) a b (- n 1))
        ))
  (f-iter 2 1 0 n))

; 1.12
; ========================================================================

(define (pascals-triangle row element)
  (cond ((= 1 element) 1)
	((= row element) 1)
	(else (+
	       (pascals-triangle (- row 1) (- element 1))
	       (pascals-triangle (- row 1) element)))))


; 1.13
; ========================================================================
; Our group agreed to skip this.

; 1.14
; ========================================================================

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount (- kinds-of-coins 1))
		 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (count-change amount)
  (cc amount 5))

; (cc 11 5)
; (cc 11 4) + (cc (11 - 50) 5)
; (cc 11 3) + (cc (11 - 25) 4) + (cc -39 5)
; (cc 11 3) + (cc -14 4) + 0
; (cc 11 2) + (cc (11 - 10) 3) + 0 + 0
; (cc 11 2) + (cc 1 3) + 0 + 0
; (cc 11 1) + (cc (11 - 5) 2) + (cc 1 2) + (cc (1 - 10) 3) + 0 + 0
; (cc 11 1) + (cc 6 2) + (cc 1 2) + (cc -9 3) + 0 + 0
; (cc 11 0) + (cc (11 - 1) 1) + (cc 6 1) + (cc (6 - 5) 2) + (cc 1 1) + (cc (1 - 5) 2) + 0 + 0 + 0
; 0 + (cc 10 1) + (cc 6 1) + (cc 1 2) + (cc 1 1) + (cc -4 2) + 0 + 0 + 0
; 0 + (cc 10 0) + (cc (10 - 1) 1) + (cc 6 0) + (cc (6 - 1) 1) + (cc 1 1) + (cc (1 - 5) 2) + (cc 1 0) + (cc (1 - 1) 1) + 0 + 0 + 0 + 0
; 0 + 0 + (cc 9 1) + 0 + (cc 5 1) + (cc 1 0) + (cc (1 - 1) 1) + (cc -4 2) + 0 + (cc 0 1) + 0 + 0 + 0 + 0
; 0 + 0 + (cc 9 0) + (cc (9 - 1) 1) + 0 + (cc 5 0) + (cc (5 - 1) 1) + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + (cc 8 1) + 0 + 0 + (cc 4 1) + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + (cc 8 0) + (cc (8 - 1) 1) + 0 + 0 + (cc 4 0) + (cc (4 - 1) 1) + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + (cc 7 1) + 0 + 0 + 0 + (cc 3 1) + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + (cc 7 0) + (cc (7 - 1) 1) + 0 + 0 + 0 + (cc 3 0) + (cc (3 - 1) 1) + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + 0 + (cc 6 1) + 0 + 0 + 0 + 0 + (cc 2 1) + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + 0 + (cc 6 0) + (cc (6 - 1) 1) + 0 + 0 + 0 + 0 + (cc 2 0) + (cc (2 - 1) 1) + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + 0 + 0 + (cc 5 1) + 0 + 0 + 0 + 0 + 0 + (cc 1 1) + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + 0 + 0 + (cc 5 0) + (cc (5 - 1) 1) + 0 + 0 + 0 + 0 + 0 + (cc 1 0) + (cc (1 - 1) 1) + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + 0 + 0 + 0 + (cc 4 1) + 0 + 0 + 0 + 0 + 0 + 0 + (cc 0 1) + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + 0 + 0 + 0 + (cc 4 0) + (cc (4 - 1) 1) + 0 + 0 + 0 + 0 + 0 + 0 + 1 + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + (cc 3 1) + 0 + 0 + 0 + 0 + 0 + 0 + 1 + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + (cc 3 0) + (cc (3 - 1) 1) + 0 + 0 + 0 + 0 + 0 + 0 + 1 + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + (cc 2 1) + 0 + 0 + 0 + 0 + 0 + 0 + 1 + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + (cc 2 0) + (cc (2 - 1) 1) + 0 + 0 + 0 + 0 + 0 + 0 + 1 + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + (cc 1 1) + 0 + 0 + 0 + 0 + 0 + 0 + 1 + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + (cc 1 0) + (cc (1 - 1) 1) + 0 + 0 + 0 + 0 + 0 + 0 + 1 + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + (cc 0 1) + 0 + 0 + 0 + 0 + 0 + 0 + 1 + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 + 1 + 0 + 0 + 0 + 0 + 0 + 0 + 1 + 0 + 1 + 0 + 0 + 1 + 0 + 0 + 0 + 0
; 1 + 1 + 1 + 1
; 4
; The order of space and step growth appears to be exponential?

; 1.15
; ========================================================================
(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; a) p is applied 6 times
; b) Space and steps grow linearly with a

; 1.16
; ========================================================================
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt base power)
  (cond ((= power 0) 1)
	((even? power) (square (fast-expt base (/ power 2))))
	(else (* base (fast-expt base (- power 1))))))

(define (fast-expt-iterative base power)
  (define (fast-expt-iter base power a)
    (cond ((= power 0) a)
          ((even? power)
	   (let ((sqr (* base base)))
	   (fast-expt-iter sqr (- (/ power 2) 1) (* a sqr))))
          (else (fast-expt-iter base (- power 1) (* a base)))
    ))
  (fast-expt-iter base power 1)
)

; 1.17
; ========================================================================
(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (fast-multiply a b)
  (cond ((= 1 a) b)
        ((even? a) (fast-multiply (halve a) (double b)))
        (else (+ b (fast-multiply (halve (- a 1)) (double b))))))

; 1.18
; ========================================================================

(define (fast-multiply-iterative a b)
  (define (fast-multiply-iter a b additions)
    (cond ((= 1 a) (+ b additions))
	  ((even? a) (fast-multiply-iter (halve a) (double b) additions))
	  (else (fast-multiply-iter (halve (- a 1)) (double b) (+ b additions)))
    ))
  (fast-multiply-iter a b 0)
)

; 1.19
; ========================================================================

(define (fib n)
  (define (fib-iter a b p q count)
     (cond ((= count 0) b)
	  ((even? count)
	   (fib-iter a
		     b
		     (+ (square p) (square q))
		     (+ (square q) (* 2 q p))
		     (/ count 2)))
	  (else (fib-iter (+ (* b q) (* a q) (* a p))
			  (+ (* b p) (* a q))
			  p
			  q
			  (- count 1)))
	  )
    )
    (fib-iter 1 0 0 1 n)
)

; 1.20
; ========================================================================
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Normal-order evaluation
; (gcd 206 40)
; (if (= 40 0))
; (gcd 40 (remainder 206 40))

; a: 40 b: (remainder 206 40)
; (if (= (remainder 206 40) 0))  ; +1
; (if (= 6) 0)
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))

; a: (remainder 206 40) b: (remainder 40 (remainder 206 40))
; (if (= (remainder 40 (remainder 206 40)) 0)) ; +1
; (if (= (remainder 40 6) 0)) ; +1
; (if (= 4 0))
; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))

; a: (remainder 40 (remainder 206 40)) b: (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ; +1
; (if (= (remainder (remainder 206 40) (remainder 40 6)) 0) ; +1
; (if (= (remainder 6 (remainder 40 6)) 0) ; +1
; (if (= (remainder 6 4) 0) ; +1
; (if (= 2 0))
; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

; a: (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) b: (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)) ;+1
; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 6))) 0)) ;+1
; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) 4)) 0)) ;+1
; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder 6 4)) 0)) ;+1
; (if (= (remainder (remainder 40 (remainder 206 40)) 2) 0)) ;+1
; (if (= (remainder (remainder 40 6) 2) 0)) ;+1
; (if (= (remainder 4 2) 0)) ;+1
; (if (= 0 0))
; (remainder (remainder 206 40) (remainder 40 (remainder 206 40)) ;+1
; (remainder (remainder 206 40) (remainder 40 6) ;+1
; (remainder (remainder 206 40) 4) ;+1
; (remainder 6 4) ;+1
; 2
; remainder is invoked 18 times!


; Applicative-order evaluation
; (gcd 206 40)
; (if (= 40 0))
; (gcd 40 (remainder 206 40)) ;+1

; (gcd 40 6)
; (if (= 6 0))
; (gcd 6 (remainder 40 6)) ;+1

; (gcd 6 4)
; (if (= 4 0))
; (gcd 4 (remainder 6 4)) ;+1

; (gcd 4 2)
; (if (= 2 0))
; (gcd 2 (remainder 4 2)) ;+1

; (gcd 2 0)
; (if (= 0 0))
; 2
; remainder is invoked 4 times

; 1.21
; ========================================================================
(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))

  (find-divisor n 2)
)

;1 ]=> (smallest-divisor 199)
;Value: 199

;1 ]=> (smallest-divisor 1999)
;Value: 1999

;1 ]=> (smallest-divisor 19999)
;Value: 7

; 1.22
; ========================================================================
(define (prime? n)
  (= n (smallest-divisor n)))

(define (report-prime n elapsed-time)
  (display (list n "is prime"))
  (display (list "elapsed time:" elapsed-time))
  (newline)
  #t)

(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (prime? n)
	(report-prime n (- (runtime) start-time))
	#f))

  (start-prime-test n (runtime))
)

(define (search-for-primes start end)
  (define (search-for-primes-iter start end num-primes-found)
    (cond ((= 3 num-primes-found) #t)
	  ((timed-prime-test start) (search-for-primes-iter (+ 1 start) end (+ 1 num-primes-found)))
	  ((= start end) #f)
	  (else (search-for-primes-iter (+ 1 start) end num-primes-found))))

  (search-for-primes-iter start end 0)
)

; Find the first 3 primes greater than 1000.
;1 ]=> (search-for-primes 1000 9999)
;(1009 is prime)(elapsed time: 0.)
;(1013 is prime)(elapsed time: 0.)
;(1019 is prime)(elapsed time: 0.)

; Find the first 3 primes greater than 10,000
;1 ]=> (search-for-primes 10000 20000)
;(10007 is prime)(elapsed time: 0.)
;(10009 is prime)(elapsed time: 0.)
;(10037 is prime)(elapsed time: 0.)

; Find the first 3 primes greater than 100,000
;1 ]=> (search-for-primes 100000 200000)
;(100003 is prime)(elapsed time: 0.)
;(100019 is prime)(elapsed time: 0.)
;(100043 is prime)(elapsed time: 0.)

; Find the first 3 primes greater than 1,000,000
;1 ]=> (search-for-primes 1000000 2000000)
;(1000003 is prime)(elapsed time: 0.)
;(1000033 is prime)(elapsed time: 0.)
;(1000037 is prime)(elapsed time: 0.)

; The (runtime) function doesn't appear to be granular enough to
; let me build meaningful process times off of it.

; 1.23
; ========================================================================
(define (next n)
  (if (= n 2)
      3
      (+ 2 n)))

(define (fast-smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (next test-divisor)))))

  (find-divisor n 2)
)

(define (fast-prime? n)
  (= n (fast-smallest-divisor n)))

(define (fast-timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (fast-prime? n)
	(report-prime n (- (runtime) start-time))
	#f))

  (start-prime-test n (runtime))
)

(define (fast-search-for-primes start end)
  (define (fast-search-for-primes-iter start end num-primes-found)
    (cond ((= 3 num-primes-found) #t)
	  ((fast-timed-prime-test start) (fast-search-for-primes-iter (+ 1 start) end (+ 1 num-primes-found)))
	  ((= start end) #f)
	  (else (fast-search-for-primes-iter (+ 1 start) end num-primes-found))))

  (fast-search-for-primes-iter start end 0)
)

; Find the first 3 primes greater than 1000.
;1 ]=> (fast-search-for-primes 1000 9999)
;(1009 is prime)(elapsed time: 0.)
;(1013 is prime)(elapsed time: 0.)
;(1019 is prime)(elapsed time: 0.)

; Find the first 3 primes greater than 10,000
;1 ]=> (fast-search-for-primes 10000 20000)
;(10007 is prime)(elapsed time: 0.)
;(10009 is prime)(elapsed time: 0.)
;(10037 is prime)(elapsed time: 0.)

; Find the first 3 primes greater than 100,000
;1 ]=> (fast-search-for-primes 100000 200000)
;(100003 is prime)(elapsed time: 0.)
;(100019 is prime)(elapsed time: 0.)
;(100043 is prime)(elapsed time: 0.)

; Find the first 3 primes greater than 1,000,000
;1 ]=> (fast-search-for-primes 1000000 2000000)
;(1000003 is prime)(elapsed time: 0.)
;(1000033 is prime)(elapsed time: 0.)
;(1000037 is prime)(elapsed time: 0.)

; As with 1.22, the (runtime) function doesn't appear
; to be granular enough to let me build meaningful process
; times off of it.

; 1.24
; ========================================================================
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fermat-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fermat-prime? n (- times 1)))
        (else false)))

(define (fermat-timed-prime-test n)
  (define (report-prime elapsed-time)
    (display (list n "is prime"))
    (display (list "elapsed time:" elapsed-time))
    (newline)
    #t)
  (define (start-prime-test n start-time)
    (if (fermat-prime? n 10)
	(report-prime (- (runtime) start-time))
	#f))

  (start-prime-test n (runtime))
)

(define (fermat-search-for-primes start end)
  (define (fermat-search-for-primes-iter start end num-primes-found)
    (cond ((= 3 num-primes-found) #t)
	  ((fermat-timed-prime-test start) (fermat-search-for-primes-iter (+ 1 start) end (+ 1 num-primes-found)))
	  ((= start end) #f)
	  (else (fermat-search-for-primes-iter (+ 1 start) end num-primes-found))))

  (fermat-search-for-primes-iter start end 0)
)

; Find the first 3 primes greater than 1000.
;1 ]=> (fermat-search-for-primes 1000 9999)
;(1009 is prime)(elapsed time: 0.)
;(1013 is prime)(elapsed time: 0.)
;(1019 is prime)(elapsed time: 0.)

; Find the first 3 primes greater than 10,000
;1 ]=> (fermat-search-for-primes 10000 20000)
;(10007 is prime)(elapsed time: 0.)
;(10009 is prime)(elapsed time: 0.)
;(10037 is prime)(elapsed time: 0.)

; Find the first 3 primes greater than 100,000
;1 ]=> (fermat-search-for-primes 100000 200000)
;(100003 is prime)(elapsed time: 0.)
;(100019 is prime)(elapsed time: 0.)
;(100043 is prime)(elapsed time: 0.)

; Find the first 3 primes greater than 1,000,000
;1 ]=> (fermat-search-for-primes 1000000 2000000)
;(1000003 is prime)(elapsed time: 0.)
;(1000033 is prime)(elapsed time: 0.)
;(1000037 is prime)(elapsed time: 0.)

; As with 1.23, the (runtime) function doesn't appear
; to be granular enough to let me build meaningful process
; times off of it.

; 1.25
; ========================================================================
; Alyssa P. Hacker is WRONG!  The advantage of our implementation is that
; instead of having to compute full on (base^n) % n, our version first
; calculates much smaller exponentiations % n and percolates those back up
; the call chain.
; Consider (2^8)%8 - in Alyssa's version we wind up evaluating full-on
; 256 % 8 whereas our version it gets reduced to (roughly)
; (remainder
;  ...
;   (remainder
;     (remainder 2 * 2^0, 8)
;   ^2, 8)
; ^2, 8)
; So the innermost remainder call only only has to deal with 2%8
; and the next remainder call only has to deal with (2^2)%8
; and subsequent remainder calls get to deal with 0s all the way up.

; 1.26
; ========================================================================
; The evaluator has to compute both calls to (expmod b n/2 n) before it can
; multiply them together.  This gets rid of the successive squaring that
; was saving us so many steps!  And you wind up having to compute several
; of the same values over and over again.

; 1.27
; ========================================================================
(define (exhaustive-fermat-prime? n)
  (define (exhaustive-fermat-prime?-iter n a)
    (define (try-it a)
      (= (expmod a n n) a))

    (cond
     ((= n a) true)
     ((try-it a) (exhaustive-fermat-prime?-iter n (+ 1 a)))
     (else false)))

  (exhaustive-fermat-prime?-iter n 1)
)

;1 ]=> (exhaustive-fermat-prime? 561)
;Value: #t
;1 ]=> (exhaustive-fermat-prime? 1105)
;Value: #t
;1 ]=> (exhaustive-fermat-prime? 1729)
;Value: #t
;1 ]=> (exhaustive-fermat-prime? 2821)
;Value: #t
;1 ]=> (exhaustive-fermat-prime? 6601)
;Value: #t

;Contrast with a non-prime non-Carmichael number
;1 ]=> (exhaustive-fermat-prime? 1106)
;Value: #f

; 1.28
; ========================================================================
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

(define (miller-rabin-timed-prime-test n)
  (define (report-prime elapsed-time)
    (display (list n "is prime"))
    (display (list "elapsed time:" elapsed-time))
    (newline)
    #t)
  (define (start-prime-test n start-time)
    (if (miller-rabin-prime? n 10)
	(report-prime (- (runtime) start-time))
	#f))

  (start-prime-test n (runtime))
)

;1 ]=> (miller-rabin-timed-prime-test 17)
;(17 is prime)(elapsed time: 0.)
;Value: #t

;1 ]=> (miller-rabin-timed-prime-test 6601)
;;Value: #f
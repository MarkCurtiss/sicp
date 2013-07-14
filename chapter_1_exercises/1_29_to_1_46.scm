; 1.29
; ========================================================================
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))

  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpsons-integral f a b n)
  (let ((h (/ (- b a) n)))
    (define (term k)
      (cond ((or (= k 0) (= k b)) (f (+ a (* k h))))
	    ((even? k) (* 2 (f (+ a (* k h)))))
	    (else (* 4 (f (+ a (* k h)))))))

    (define (next-k a)
      (+ 1 a))

    (* (/ h 3) (sum term a next-k n))))


;1 ]=> (integral cube 0 1 .01)
;Value: .24998750000000042
;1 ]=> (integral cube 0 1 .001)
;Value: .249999875000001


;1 ]=> (simpsons-integral cube 0 1 100)
;Value: 75999997/300000000
; Calculator resolves this as 0.25333332333333

;1 ]=> (simpsons-integral cube 0 1 1000)
;Value: 750999999997/3000000000000
; Calculator resolves this as 0.25033333333233

; 1.30
; ========================================================================
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))

  (iter a 0))

; 1.31
; ========================================================================
; a.
(define (identity x) x)

(define (inc x) (+ 1 x))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial x)
  (product identity 1 inc x))

(define (wallis-term x)
   (/ (* 2 x)
      (- (* 2 x) 1))
   (/ (* 2 x)
      (+ (* 2 x) 1)))

; According to my calculator, π = 3.141592653589793
;
;1 ]=> (* 2 (product wallis-term 1.0 inc 100))
;Value: 3.13378749062816
;
;1 ]=> (* 2 (product wallis-term 1.0 inc 10000))
;Value: 3.141514118681846

; b.
(define (product-iterative term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))

  (iter a 1))


;1 ]=> (product identity 0 inc 100000)
;Aborting!: maximum recursion depth exceeded

;1 ]=> (product-iterative identity 0 inc 100000)
;Value: 0

; 1.32
; ========================================================================
; a.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

; sum
;1 ]=> (accumulate + 0 identity 1 inc 30)
;Value: 465

; product
;1 ]=> (accumulate * 1 identity 1 inc 6)
;Value: 720

; b.
(define (accumulate-iterative combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (term a)))))

  (iter a null-value))

;1 ]=> (accumulate + 0 identity 1 inc 1000000)
;Aborting!: maximum recursion depth exceeded

;1 ]=> (accumulate-iterative + 0 identity 1 inc 1000000)
;Value: 500000500000


; 1.33
; ========================================================================
; Despite all the time we spent writing faster, smarter prime? routines
; I'm using this simple one to keep the number of lines of code down.
; Hopefully in a future lesson we learn to define libaries.
(define (prime? n)
  (define (smallest-divisor n)
    (define (divides? a b)
      (= (remainder b a) 0))

    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
	    ((divides? test-divisor n) test-divisor)
	    (else (find-divisor n (+ test-divisor 1)))))

    (find-divisor n 2)
    )

  (if (= 1 n)
      false
      (= n (smallest-divisor n))))

(define (filtered-accumulate filter combiner null-value term a next b)
  (if (filter a)
      (if (> a b)
	  null-value
	  (combiner (term a)
		    (filtered-accumulate filter combiner null-value term (next a) next b)))
      (filtered-accumulate filter combiner null-value term (next a) next b)))

;a.
;1 ]=> (filtered-accumulate prime? + 0 square 1 inc 10)
;Value: 87

;b.
(define (product-of-relatively-prime-integers n)
  (define (relatively-prime? a)
    (= 1 (gcd a n)))

  (filtered-accumulate relatively-prime? * 1 identity 1 inc n))

;1 ]=> (product-of-relatively-prime-integers 10)
;Value: 189

; 1.34
; ========================================================================
(define (f g)
  (g 2))
; Recursion!  (f f) gets called and the argument f gets evaluated
; with (f 2).  You then wind up calling (f g) with 2 as the second argument
; which resolves to (2 2).  2 can't be called on 2!

;1 ]=> (f f)
;The object 2 is not applicable.

; 1.35
; ========================================================================
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))


;1 ]=> (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;Value: 1.6180327868852458

;1 ]=> (/ (+ 1 (sqrt 5)) 2)
;Value: 1.618033988749895

; 1.36
; ========================================================================
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display (list "Now comparing guess" guess "to next" next))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;1 ]=> (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)
;(Now comparing guess 1.1 to next 72.47657378429035)
;(Now comparing guess 72.47657378429035 to next 1.6127318474109593)
;(Now comparing guess 1.6127318474109593 to next 14.45350138636525)
;(Now comparing guess 14.45350138636525 to next 2.5862669415385087)
;(Now comparing guess 2.5862669415385087 to next 7.269672273367045)
;(Now comparing guess 7.269672273367045 to next 3.4822383620848467)
;(Now comparing guess 3.4822383620848467 to next 5.536500810236703)
;(Now comparing guess 5.536500810236703 to next 4.036406406288111)
;(Now comparing guess 4.036406406288111 to next 4.95053682041456)
;(Now comparing guess 4.95053682041456 to next 4.318707390180805)
;(Now comparing guess 4.318707390180805 to next 4.721778787145103)
;(Now comparing guess 4.721778787145103 to next 4.450341068884912)
;(Now comparing guess 4.450341068884912 to next 4.626821434106115)
;(Now comparing guess 4.626821434106115 to next 4.509360945293209)
;(Now comparing guess 4.509360945293209 to next 4.586349500915509)
;(Now comparing guess 4.586349500915509 to next 4.535372639594589)
;(Now comparing guess 4.535372639594589 to next 4.568901484845316)
;(Now comparing guess 4.568901484845316 to next 4.546751100777536)
;(Now comparing guess 4.546751100777536 to next 4.561341971741742)
;(Now comparing guess 4.561341971741742 to next 4.551712230641226)
;(Now comparing guess 4.551712230641226 to next 4.558059671677587)
;(Now comparing guess 4.558059671677587 to next 4.55387226495538)
;(Now comparing guess 4.55387226495538 to next 4.556633177654167)
;(Now comparing guess 4.556633177654167 to next 4.554812144696459)
;(Now comparing guess 4.554812144696459 to next 4.556012967736543)
;(Now comparing guess 4.556012967736543 to next 4.555220997683307)
;(Now comparing guess 4.555220997683307 to next 4.555743265552239)
;(Now comparing guess 4.555743265552239 to next 4.555398830243649)
;(Now comparing guess 4.555398830243649 to next 4.555625974816275)
;(Now comparing guess 4.555625974816275 to next 4.555476175432173)
;(Now comparing guess 4.555476175432173 to next 4.555574964557791)
;(Now comparing guess 4.555574964557791 to next 4.555509814636753)
;(Now comparing guess 4.555509814636753 to next 4.555552779647764)
;(Now comparing guess 4.555552779647764 to next 4.555524444961165)
;(Now comparing guess 4.555524444961165 to next 4.555543131130589)
;(Now comparing guess 4.555543131130589 to next 4.555530807938518)
;(Now comparing guess 4.555530807938518 to next 4.555538934848503)
;Value: 4.555538934848503
; This took 37 steps.

;1 ]=> (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 1.1)
;(Now comparing guess 1.1 to next 36.78828689214517)
;(Now comparing guess 36.78828689214517 to next 19.352175531882512)
;(Now comparing guess 19.352175531882512 to next 10.84183367957568)
;(Now comparing guess 10.84183367957568 to next 6.870048352141772)
;(Now comparing guess 6.870048352141772 to next 5.227224961967156)
;(Now comparing guess 5.227224961967156 to next 4.701960195159289)
;(Now comparing guess 4.701960195159289 to next 4.582196773201124)
;(Now comparing guess 4.582196773201124 to next 4.560134229703681)
;(Now comparing guess 4.560134229703681 to next 4.5563204194309606)
;(Now comparing guess 4.5563204194309606 to next 4.555669361784037)
;(Now comparing guess 4.555669361784037 to next 4.555558462975639)
;(Now comparing guess 4.555558462975639 to next 4.55553957996306)
;(Now comparing guess 4.55553957996306 to next 4.555536364911781)
;Value: 4.555536364911781
; This took 13 steps.

; 1.37
; ========================================================================
; a.
(define (cont-frac n d k)
  (define (iter n d k i)
    (if (= i k)
	(/ (n i) (d i))
	(/ (n i) (+ (d i) (iter n d k (+ 1 i))))))

  (iter n d k 1))

; The target number is:
;1 ]=> (/ 1 1.618033988749895)
;Value: .6180339887498948

; It takes a k of 11 to get an approximation accurate to 4 decimal places:
;1 ]=> (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
;Value: .6180555555555556

; b.
(define (cont-frac-iterative n d k)
  (define (iter n d k i result)
    (cond ((= i k) (iter n d k (- i 1) (/ (n k) (d k))))
	  ((= i 0) result)
	  (else (iter n d k (- i 1) (/ (n k) (+ (d k) result))))))

  (iter n d k k 0))

;1 ]=> (cont-frac-iterative (lambda (i) 1.0) (lambda (i) 1.0) 11)
;Value: .6180555555555556

; 1.38
; ========================================================================
(define (euler-sequence i)
  (cond ((= i 2) 2)
	((= (remainder i 3) 2) (+ 2 (test-it (- i 3))))
	(else 1)))

;1 ]=> (+ 2.0 (cont-frac (lambda (i) 1.0) euler-sequence 10))
;Value: 2.7182817182817183

; 1.39
; ========================================================================
(define (tan-cf x k)
  (cont-frac
   (lambda (i)
     (if (= i 1)
	 x
	 (- (square x))))
   (lambda (i) (- (* 2.0 i) 1.0))
   k))


; Compare to the built-in
;1 ]=> (tan 1.2)
;Value: 2.5721516221263183

;1 ]=> (tan-cf 1.2 10)
;Value: 2.5721516221263188

; 1.40
; ========================================================================
(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+
     (cube x)
     (* a (square x))
     (* b x)
     c)
    ))

;1 ]=> (let
;    ((a 1)
;     (b 2)
;     (c 3))
;  (newtons-method (cubic a b c) 1))
;
;(Now comparing guess 1 to next 5.714266429257542e-6)
;(Now comparing guess 5.714266429257542e-6 to next -1.4999839284734562)
;(Now comparing guess -1.4999839284734562 to next -1.3043428073335799)
;(Now comparing guess -1.3043428073335799 to next -1.276209090923146)
;(Now comparing guess -1.276209090923146 to next -1.27568238137649)
;(Now comparing guess -1.27568238137649 to next -1.2756822036498454)
;Value: -1.2756822036498454

; 1.41
; ========================================================================
(define (double f)
  (lambda (x)
    (f (f x))
  ))

(define (inc i) (+ 1 i))

;1 ]=> (((double (double double)) inc) 5)
;Value: 21

; 1.42
; ========================================================================
(define (compose f g)
  (lambda (x)
    (f (g x))))


;1 ]=> ((compose square inc) 6)
;Value: 49

; 1.43
; ========================================================================
(define (repeated function num)
  (define (iter i)
    (if (= i num)
	(lambda (x) (function x))
	(compose function (iter (+ 1 i)))))

  (iter 1))

;1 ]=> ((repeated square 2) 5)
;Value: 625

; 1.44
; ========================================================================
(define (smooth function)
  (lambda (x)
    (/
      (+
       (function (- x dx))
       (function x)
       (function (+ x dx))
       )
      3)
    ))


;1 ]=> ((smooth square) 2)
;Value: 4.000000000066667

(define (n-fold-smooth function n)
  ((repeated smooth n) function))

;1 ]=> ((n-fold-smooth square 2) 2)
;Value: 4.000000000133333

; 1.45
; ========================================================================
(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (log2 x)
  (/
   (log x)
   (log 2)))

(define (nth-root x n)
  (let ((num-applications (truncate (log2 n))))
    (fixed-point
     ((repeated average-damp num-applications)
      (lambda (y) (/ x (expt y (- n 1)))))
     1.0)))

;1 ]=> (nth-root 256 8)
;(Now comparing guess 1. to next 32.875)
;(Now comparing guess 32.875 to next 28.765625000771063)
;(Now comparing guess 28.765625000771063 to next 25.16992187763819)
;(Now comparing guess 25.16992187763819 to next 22.023681647933493)
;(Now comparing guess 22.023681647933493 to next 19.270721454674508)
;(Now comparing guess 19.270721454674508 to next 16.861881305264014)
; ...
;(Now comparing guess 2.0009281883446266 to next 2.0000015055871168)
;(Now comparing guess 2.0000015055871168 to next 2.000000000003967)
;Value: 2.000000000003967

; 1.46
; ========================================================================
(define (sqrt x)
  ((iterative-improve
    (lambda (guess) (< (abs (- (square guess) x)) 0.001))
    (lambda (guess) (average guess (/ x guess))))
   x))

;1 ]=> (sqrt 81.0)
;Value: 9.000011298790216

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
	guess
	(iter (improve guess))
	))
  iter)

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (guess) (< (abs (- guess (f guess))) 0.00001))
    (lambda (guess) (f guess)))
     first-guess))

;1 ]=> (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;Value: 1.6180371352785146
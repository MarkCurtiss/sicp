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


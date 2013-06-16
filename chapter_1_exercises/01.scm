; 1.1
; ========================================================================
; 10
; 12
; 8
; 3
; 6
; a
; b
; 19
; #f
; 4
; 16
; 6
; 16

; 1.2
; ========================================================================
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (* (- 6 2) (- 2 7)))
)
;Value: -37/150

; 1.3
; ========================================================================
(define (sum-of-squares x y)
  (+ (* x x) (* y y))
)

(define (min-of-three x y z)
  (cond ((and (< x y) (< x z) x))
	((and (< y x) (< y z) y))
	(else z)
  )
)

(define (sum-of-largest-squares x y z)
  (define smallest (min-of-three x y z))
  (cond ((= smallest x) (sum-of-squares y z))
	((= smallest y) (sum-of-squares x z))
	(else (sum-of-squares y z))
  )
)
; > (sum-of-largest-squares 5 4 6)
;Value: 61

; 1.4
; ========================================================================
; This returns the value of a + the absolute value of b.  So if b is > 0,
; a will be added to the the original value of b.
; Otherwise, a will subtract the value of b but since b is negative this
; evaluates to addition (a - -b).

; 1.5
; ========================================================================
; In an applicative-order evaluation the interpreter will return 0 -
; the interpreter will first evaluate the call to 'test' and return 0
; because x == 0 will evaluate to true.
; In a normal-order evaluation the interpreter will run indefinitely -
; it'll first replace 'test' with the definition of test without incident, 
; but when it attempts to expand 'p' it will enter an infinite loop of
; expansion.

; 1.6
; ========================================================================
; I think this will work the same way!  'new-if' will be the first operator
; expanded when 'sqrt-iter' is called and it will only evaluate 'else-clause'
; when necessary, preventing infinite recursion.  If we were using normal-
; order evaluation, though, it would bomb out.

; 1.7
; ========================================================================
(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))
; With very small numbers, virtually anything we pass into good-enough?
; will pass the threshold of .0001 and pass the test.  This is because
; the difference between the square of any tiny guess and our actual number
; will be less than .0001:

; 1 ]=> (square .0008)
; Value: .00000064

; 1 ]=> (sqrt .00000064)
; Value: .03125681970321202

; 1 ]=> (good-enough? .00004 .00000064)
; Value: #t

; 1 ]=> (good-enough? .0000000000000000004 .00000064)
; Value: #t


; With large numbers, the limited precision available means that our mantissa
; can get straight up squared out of existence (beyond the OSs precision) and
; make an incorrect guess seem correct!  Observe:

; 1 ]=> (good-enough? 3500000000.000000009 12250000000000000000)
; Value: #t

(define (better-good-enough? prev-guess guess x)
  (< (/ (abs (- guess prev-guess)) guess) 0.0000000000001))

(define (better-sqrt-iter prev-guess guess x)
  (if (better-good-enough? prev-guess guess x)
      guess
      (better-sqrt-iter guess (improve guess x) x)))
  
(define (better-sqrt x)
  (better-sqrt-iter 1.0 (improve 1.0 x) x))

; Small numbers:
; 1 ]=> (better-sqrt .00000064)
; Value: 7.999999999999999e-4

; Large numbers
; 1 ]=> (better-sqrt 12250000000000000000)
; Value: 3500000000.


; 1.8
; ========================================================================

(define (cube x)
  (* x x x))

(define (cube-good-enough? prev-guess guess x)
  (< (/ (abs (- guess prev-guess)) guess) 0.0000000000001))
  
(define (improve-root guess x)
   (/ 
    (+ (/ x (square guess)) (* 2 guess))
    3))

(define (cube-root-iter prev-guess guess x)
  (if (cube-good-enough? prev-guess guess x)
      guess
      (cube-root-iter guess (improve-root guess x) x)))
  
(define (cube-root x)
  (cube-root-iter 1.0 (improve-root 1.0 x) x))


; 1 ]=> (cube-root 27)
; Value: 3.
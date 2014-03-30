; 3.50
; ========================================================================
(define (multi-stream-map proc . argstreams)
  (if (null? (stream-car (car argstreams)))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply multi-stream-map
              (cons proc (map stream-cdr argstreams))))))

; 3.51
; ========================================================================
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

;; 1 ]=> (define x (stream-map show (stream-enumerate-interval 0 10)))
;; 0
;; ;Value: x

;; 1 ]=> (stream-ref x 5)
;; 1
;; 2
;; 3
;; 4
;; 5
;; ;Value: 5

;; 1 ]=> (stream-ref x 7)
;; 6
;; 7
;; ;Value: 7

; 3.52
; ========================================================================
;; 1 ]=> (define sum 0)
;; ;Value: sum

;; 1 ]=> (define (accum x)
;;   (set! sum (+ x sum))
;;   sum)
;; ;Value: accum

;; 1 ]=> (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;; ;Value: seq

;; 1 ]=> (define y (stream-filter even? seq))
;; ;Value: y

;; 1 ]=> (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;;                          seq))
;; ;Value: z

;; 1 ]=> (stream-ref y 7)
;; ;Value: 136

;; 1 ]=> (display-stream z)
;; 10
;; 15
;; 45
;; 55
;; 105
;; 120
;; 190
;; 210
;; ;Unspecified return value

;; If our implementation of (delay) wasn't memo-ized, (stream-ref y 7) would still
;; produce the same result but (display-stream z) wouldn't.  Since the variable
;; sum is getting set as a side effect of mapping over the stream, iterating over z
;; after iterating over y means you'd be essentially starting at the 8th element
;; in y's sequence.
;; In the memo-ized version, each element of the stream is only getting set once
;; so you get the same result every time you map over it.

; 3.53
; ========================================================================
;; The stream's elements look like
;; 0: 1
;; 1: (1 + 1, (delay add-streams s s)) = 2
;; 2: (1 + 1) + (1 + 1), (delay add-streams s s) = 4
;; 3: (1 + 1 + 1 + 1) + (1 + 1 + 1 + 1), (delay add-streams s s) = 8
;; Each element is double the previous element.

; 3.54
; ========================================================================
(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (first-n-elements-of-stream stream n)
  (define (iter list s index)
    (if (= index n)
	list
	(iter (cons (stream-car s) list)
	      (stream-cdr s)
	      (+ index 1))))

  (reverse (iter '() stream 0)))

(define factorial (cons-stream 1 (mul-streams (stream-cdr integers) factorial)))

; 3.55
; ========================================================================
(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

; 3.56
; ========================================================================
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge
			  (scale-stream S 2)
			  (merge
			   (scale-stream S 3)
			   (scale-stream S 5)))))

; 3.57
; ========================================================================
;; You perform n - 1 additions to compute the nth Fibonacci number.
;; If we didn't cache results, each Fibonacci number would have to recompute
;; each Fibonacci number that came before it.
;; So (fib 5) would have to compute (fib 4) (fib 3)... and (fib 4) would
;; have to compute (fib 3) (fib 2)...
;; That looks awful exponential to me!

; 3.58
; ========================================================================
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))
;; This stream divides the numerator by the denomiator in that radix
;; and returns the decimal representation one digit at a time.
;; For example, (expand 5 20 10) == (2 5 0 0 0 ...) == .25000

;; 1 ]=> (stream-ref (expand 1 7 10) 0)
;; ;Value: 1
;; 1 ]=> (stream-ref (expand 1 7 10) 1)
;; ;Value: 4
;; 1 ]=> (stream-ref (expand 1 7 10) 2)
;; ;Value: 2

;; 1 ]=> (stream-ref (expand 3 8 10) 0)
;; ;Value: 3
;; 1 ]=> (stream-ref (expand 3 8 10) 1)
;; ;Value: 7
;; 1 ]=> (stream-ref (expand 3 8 10) 2)
;; ;Value: 5

; 3.59
; ========================================================================
;; a.
(define (integrate-series power-series)
  (stream-map / power-series integers))

;; b.
(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))

(define sine-series
  (cons-stream 1 (integrate-series cosine-series)))

; 3.60
; ========================================================================
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams
		(mul-series (stream-cdr s1)
			    (stream-cdr s2)))))

; 3.61
; ========================================================================
(define (invert-unit-series S)
  (cons-stream 1 (mul-series (scale-stream (stream-cdr S) -1)
			     (invert-unit-series S))))

; 3.62
; ========================================================================
(define (div-series numerator denominator)
  (if (= (stream-car denominator) 0)
      (error "div-series cannot handle 0 denominator")
      (mul-series numerator (invert-unit-series denominator))))

(define tangent-series
  (div-series sine-series cosine-series))
; 3.63
; ========================================================================
;; Louis Reasoner's implementation will recalculate elements of the stream
;; while Alyssa P. Hacker's takes advantage of the memoization that (delay)
;; gave us.  If (delay) was implemented sans memoization there'd be no
;; difference in performance.

; 3.64
; ========================================================================
(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (first-n-elements-of-stream stream n)
  (define (iter list s index)
    (if (= index n)
	list
	(iter (cons (stream-car s) list)
	      (stream-cdr s)
	      (+ index 1))))

  (reverse (iter '() stream 0)))

(define (stream-limit stream tolerance)
  (define (iter last-guess s)
    (let ((guess (stream-car s)))
      (if (and last-guess (< (abs (- last-guess guess)) tolerance))
	  guess
	  (iter guess (stream-cdr s)))))
  (iter #f (stream-cdr stream))
  )

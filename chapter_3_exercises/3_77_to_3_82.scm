; 3.77
; ========================================================================
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral delayed-integrand initial-value dt)
    (cons-stream initial-value
		 (if (stream-null? delayed-integrand)
		     the-empty-stream
		     (let ((integrand (force delayed-integrand)))
		       (integral (delay (stream-cdr integrand))
				 (+ (* dt (stream-car integrand))
				    initial-value)
				 dt)))))

; 3.78
; ========================================================================
(define (first-n-elements-of-stream stream n)
  (define (iter list s index)
    (if (= index n)
	list
	(iter (cons (stream-car s) list)
	      (stream-cdr s)
	      (+ index 1))))

  (reverse (iter '() stream 0)))

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy
    (add-streams
     (scale-stream dy a)
     (scale-stream y b)))
  y)

; 3.79
; ========================================================================
(define (solve-2nd-with-f f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy
    (stream-map f y dy))

  y)

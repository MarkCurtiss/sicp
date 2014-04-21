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
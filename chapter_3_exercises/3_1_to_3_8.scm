; 3.1
; ========================================================================
(define (make-accumulator initial-value)
  (let ((value initial-value))
    (lambda (addend)
      (set! value (+ value addend))
      value)))

; 3.2
; ========================================================================
(define (make-monitored function)
  (let ((num-calls 0))
    (lambda (arg)
      (cond ((equal? arg 'how-many-calls) num-calls)
	    ((equal? arg 'reset-count) (set! num-calls 0))
	    (else
	     (set! num-calls (+ 1 num-calls))
	     (function arg))))))

; 3.3
; ========================================================================
(define (make-account balance password)
  (let ((user-password password))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (dispatch password m)
      (if (not (equal? password user-password))
	  (lambda (x) "Incorrect password")
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		(else (error "Unknown request -- MAKE-ACCOUNT"
			     m)))))
    dispatch))

; 3.4
; ========================================================================
(define (make-account balance password)
  (let ((user-password password)
	(num-illegal-accesses 0))

    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))

    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (call-the-cops)
      (error "Whee-oo whee-oo!  The cops have been called"))

    (define (check-password supplied-password)
      (if (not (equal? supplied-password user-password))
	  (cond ((= num-illegal-accesses 7)
		 (call-the-cops))
		(else
		 (set! num-illegal-accesses (+ 1 num-illegal-accesses))
		 false))
	  (begin (set! num-illegal-accesses 0)
		 true)))

    (define (dispatch password m)
      (if (check-password password)
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		(else (error "Unknown request -- MAKE-ACCOUNT"
			     m)))
	  (lambda (x) "Incorrect password")))

    dispatch))

; 3.5
; ========================================================================
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (area-rect x1 y1 x2 y2)
  (define (length-side x1 x2 y1 y2)
    (sqrt (+
	   (square (abs (- x2 x1)))
	   (square (abs (- y2 y1))))))

  (*
   (length-side x1 x1 y1 y2)
   (length-side x1 x2 y1 y1)))

(define (estimate-integral predicate x1 x2 y1 y2 num-trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
	  (y (random-in-range y1 y2)))

      (predicate x y)))

  (*
   (monte-carlo num-trials experiment)
   (area-rect x1 y1 x2 y2)))

; 3.6
; ========================================================================
(define random-init 0)
(define (rand-update value)
  (+ 1 value))

(define rand
  (let ((x random-init))
    (define (dispatch method)
      (cond ((eq? method 'generate)
	       (set! x (rand-update x))
	       x)
	    ((eq? method 'reset)
	     (lambda (new-seq)
	       (set! x new-seq)
	       x))))
  dispatch))

; 3.7
; ========================================================================

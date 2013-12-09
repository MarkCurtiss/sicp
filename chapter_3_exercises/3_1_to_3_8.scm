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
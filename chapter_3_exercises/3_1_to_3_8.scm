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
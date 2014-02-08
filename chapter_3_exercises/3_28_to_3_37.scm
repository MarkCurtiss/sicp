; 3.28
; ========================================================================
(define (logical-or signal1 signal2)
  (or signal1 signal2))

(define (or-gate wire1 wire2 output-wire)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal wire1) (get-signal wire2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output-wire new-value)))))
  (add-action! wire1 or-action-procedure)
  (add-action! wire2 or-action-procedure)
  'ok)

; 3.29
; ========================================================================
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-and signal1 signal2)
  (and signal1 signal2))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (or-gate wire1 wire2 output-wire)
  (let ((wire3 (make-wire))
	(wire4 (make-wire))

    (inverter wire1 wire3)
    (inverter wire2 wire4)
    (and-gate wire3 wire4 output-wire))))

;; The delay time is equal to
;; (+ inverter-delay inverter-delay and-gate-delay)

; 3.30
; ========================================================================
(define (ripple-carry-adder a-list b-list sum-list carry-in)
  (let ((carry-out (make-wire)))
    (if (null? (cdr a-list))
	(full-adder (car a-list) (car b-list) carry-in (car s-list) carry-out)
	(ripple-carry-adder (cdr a-list b-list sum-list carry-out)))))

;; The delay time will be
;; n * (2 * (half-adder + or-gate)) ==
;; n * ((2 * (or-gate + (2 * and-gate) + inverter)) + or-gate)

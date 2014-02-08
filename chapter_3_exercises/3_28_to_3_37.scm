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

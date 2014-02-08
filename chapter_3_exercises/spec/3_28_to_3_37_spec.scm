(load "3_28_to_3_37.scm")

(describe "or gate"
  (it "returns the logical OR of its inputs"
    (lambda ()
      (define wire1 (make-wire))
      (define wire2 (make-wire))
      (define output-wire (make-wire))

      (set-signal! wire1 0)
      (set-signal! wire2 0)

      (define or-gate (or-gate wire1 wire2 output-wire))

      (set-signal! wire1 1)

      (assert (equal?
	       (get-signal output-wire)
	       1))

      (set-signal! wire1 0)

      (assert (equal?
	       (get-signal output-wire)
	       0))
      ))
)
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
	(wire4 (make-wire)))

    (inverter wire1 wire3)
    (inverter wire2 wire4)
    (and-gate wire3 wire4 output-wire)))

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

; 3.31
; ========================================================================
;; If we didn't invoke the callback right away we wouldn't actually place
;; events in the agenda when we construct half-adders and or-gates and such.
;; Thus, the simulation wouldn't produce any output.

; 3.32
; ========================================================================
;; queue
;; (A 0 -> 1, B 0 -> 1, A 1 -> 0, B 1 -> 0)

;; stack
;; (B 1 -> 0, A 1 -> 0, B 0 -> 1, A 0 -> 1)

;; If a stack were used, the AND gate would appear to go (incorrectly):
;; False, False, False, True
;; If the queue is used, the AND gate properly signals:
;; False, True, False, False.

; 3.33
; ========================================================================
;; Here comes a few pages of copypasta:
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;; Okay now here is some new code.
(define (averager a b c)
  (let ((twoc (make-connector))
	(u (make-connector))
	(v (make-connector)))
    (multiplier c twoc u)
    (constant 2 twoc)
    (adder a b u))
  'ok)

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(averager A B C)

(probe "A" A)
(probe "B" B)
(probe "C" C)

(set-value! A 9 'user)
(set-value! B 11 'user)

;; Probe: A = 9
;; Probe: B = 11
;; Probe: C = 10
;; ;Value: done

; 3.34
; ========================================================================
;; Changes to the value of b won't propagate back to a.

;; 1 ]=> (define (squarer a b)
;;   (multiplier a a b))
;; Value: squarer

;; 1 ]=> (define R (make-connector))
;; Value: r

;; 1 ]=> (define S (make-connector))
;; Value: s

;; 1 ]=> (probe "R" R)
;; Value 5: #[compound-procedure 5 me]

;; 1 ]=> (probe "S" S)
;; Value 6: #[compound-procedure 6 me]

;; 1 ]=> (squarer R S)
;; Value 7: #[compound-procedure 7 me]

;; 1 ]=> (set-value! R 4 'user)
;; Probe: S = 16
;; Probe: R = 4
;; Value: done

;; So far so good.  But now, observe:
;; 1 ]=> (forget-value! R 'user)
;; Probe: S = ?
;; Probe: R = ?
;; Value: done

;; 1 ]=> (set-value! S 16 'user)
;; Probe: S = 16
;; Value: done

;; Notice that we don't see the value of R change.
;; This is because none of the conditionals in (process-new-value) in
;; (multiplier m1 m2 product) will ever be hit - although product has a value
;; neither m1 nor m2 have a value in Louis Reasoner's implementation.

; 3.35
; ========================================================================
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
	    (set-value! a (sqrt (get-value b)) me))
	(if (has-value? a)
	    (set-value! b (square (get-value a)) me))))

  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;; 1 ]=> (define R (make-connector))
;; Value: r

;; 1 ]=> (define S (make-connector))
;; Value: s

;; 1 ]=> (probe "R" R)
;; Value 8: #[compound-procedure 8 me]

;; 1 ]=> (probe "S" S)
;; Value 9: #[compound-procedure 9 me]

;; 1 ]=> (squarer R S)
;; Value 10: #[compound-procedure 10 me]

;; 1 ]=> (set-value! R 4 'user)

;; Probe: S = 16
;; Probe: R = 4
;; Value: done

;; 1 ]=> (forget-value! R 'user)
;; Probe: S = ?
;; Probe: R = ?
;; Value: done

;; 1 ]=> (set-value! S 16 'user)
;; Probe: R = 4
;; Probe: S = 16
;; Value: done

; 3.36
; ========================================================================
"
               +--------------------------------------+
global-env+--->|                                      |
               |                                      |<------------------------+
               |                                      |                         |
    +-----------+a                                  b+--------+                 |
    |          +--------------------------------------+       |                 |
    |                  ^                                      |                 |
    |                  |                                      |                 |
    |                  |                                      |         +-------+--------+
    |        +---------+------+                               v   E2+-->|value: false    |
    v  E1+-->|value: 10       |                              +-+        |informant: false|
   +-+       |informant: 'user|<-----------+                 |+-------> |constraints: () |
   |+------->|constraints: () |            |                 +-+        +----------------+
   +-+       +----------------+            |                 |+------+
 +--+|                                     |                 +-+     |
 | +-+                                     |                         |
 |                                         |                         |
 v                             +-----------+-----------------+       v
params: request          E3+-->|exception: 'user             |     params: request
body:                          |procedure: inform-about-value|     body:
  (cond ((eq? request 'has...  |list: ()                     |       (cond ((eq? request 'has...
                               +-----------------------------+
                     +-+                 ^
                     |+------------------+
                     +-+
          +-----------+|
          v          +-+
       params: exception, procedure, list
       body:
         (define (loop items) ...
"

; 3.37
; ========================================================================
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

;; 1 ]=>  (probe "Celsius temp" C)
;; Value 8: #[compound-procedure 8 me]
;; 1 ]=> (probe "Fahrenheit temp" F)
;; Value 9: #[compound-procedure 9 me]

;; 1 ]=> (set-value! C 25 'user)

;; Probe: Celsius temp = 25
;; Probe: Fahrenheit temp = 77
;; Value: done
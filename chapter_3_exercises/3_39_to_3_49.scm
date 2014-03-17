; 3.39
; ========================================================================
;; 101:P1 sets x to 100 and then P2 increments x to 101.
;; 121:P2 increments x to 11 and then P1 sets x to x times x.
;; 100:P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.

; 3.40
; ========================================================================
;; 10 * 10 == 100, 100 * 100 * 100 = 1,000,000
;; 10 * (100*100*100) ==  10,000
;; 10 * 10 * 10 = 1,000
;; 10 * 10 = 100
;; 10 * 10 * (100 * 100) = 100,000

;; If we serialize the operations, the only possibilities are:
;; 10 * 10 == 100, 100 * 100 * 100 = 1,000,000

; 3.41
; ========================================================================
;; No I do not agree.  Ben's idea is crazy.  You can't modify the result of
;; a call to ((make-account 100) 'balance) so there's no danger in allowing
;; unserialized access to it.

; 3.42
; ========================================================================
;; Jeah this seems safe.  I don't see that it would produce different results
;; than the other way of serializing those procedures.

; 3.43
; ========================================================================
;; If the processes run serially there will be no way for them to change
;; their balances to anything other than $10, $20, and $30.  How would
;; that even happen!  At every call to exchange the two accounts involved
;; swap values uninterrupted.

;; Here is a scenario where the total values of $10, $20 and $30 can be
;; violated:
"
+---------+      +---------+     +---------+
|Account 1|      |Account 2|     |Account 3|
+----+----+      +---+-----+     +---+-----+
     v               v               v
 balance 10       balance 20      balance 30
      +               +
      v               v
(exchange account 1 account 2)

+---------------+
|difference: -10|
+-----+---------+
      v
(exchange account 1                account 3)
      +
      |            +---------------+
      v            |difference: -20|
+------------+     +-----+---------+
|withdraw -10|           |
+-----+------+           |
+------------+           v
|withdraw -20|     +-----------+
+-----+------+     |deposit -10|    +-----------+
      |            +-----+-----+    |deposit -20|
      |                  |          +------+----+
      |                  |                 |
      v                  v                 v
 balance: 40        balance: 10       balance: 10
"

;; Notice that the total value is still $60, same as in the serialized
;;  version.

"
+---------+      +---------+     +---------+
|Account 1|      |Account 2|     |Account 3|
+----+----+      +---+-----+     +---+-----+
     v               v               v
 balance 10       balance 20      balance 30
      +               +
      v               v
(exchange account 1 account 2)

+---------------+
|difference: -10|
+-----+---------+
      v
(exchange account 1                account 3)
      +
      |                        +---------------+
      v                        |difference: -20|
+------------+                 +-----+---------+
|withdraw -10|                       |
+-----+------+ +------------+        |
      |        |withdraw -20|        v
      |        +-----+------+  +-----------+
      |              |         |deposit -10|    +-----------+
      v<-------------+         +-----+-----+    |deposit -20|
      |                              |          +------+----+
      |                              |                 |
      v                              v                 v
 balance: 20                    balance: 10       balance: 10
"

;; If we have another withdrawal take place in the middle of our original
;; one, the second withdrawal will be essentially erased.  Notice $20 disappeared
;; from the system!

; 3.44
; ========================================================================
;; Man this problem description is ambiguous.  I guess I should be used to
;; that by now huh?  I will assume they're referring to the (make-account)
;; implementation that provides serialized withdraw and deposit amounts and
;; the un-serialized (exchange) procedure.

;; Louis Reasoner is wrong AGAIN.  The issue with the exchange procedure
;; was that it stored some state outside of the serialized sections: namely,
;; difference between the two accounts' balances.

; 3.45
; ========================================================================
;; You will deadlock yourself!  (deposit) and (withdraw) already run inside
;; of serialized transactions but client code can also serialize transactions
;; as in the case of (serialized-exchange).  So you'll wind up blocking your
;; own transaction.

; 3.46
; ========================================================================
"
+---------+          +---------+
|process 1|          |process 2|
+---+-----+          +---+-----+
    v                    |
+--------------+         v
|(if (car cell)|     +--------------+
+---+----------+     |(if (car cell)|
    |                +-------------++
    |                              |
    |                              |
    v                              |
+----------------------------+     |
|(begin (set! (car cell true)|     v
+----------------------------+   +----------------------------+
                                 |(begin (set! (car cell true)|
                                 +----------------------------+
"

;; Now both processes have the mutex!  We can no longer reason intelligently
;; about our program's execution.

; 3.47
; ========================================================================
;; a.
(define (make-semaphore n)
  (let ((available-mutices (map make-mutex (make-list n)))
	(used-mutices '())))

  (define (mutex-available?)
    (> (length available-mutices) 0))

  (define (mutex-can-be-released?)
    (> (length used-mutices) 0))

  (define (acquire-mutex)
    (append used-mutices (last-pair available-mutices))
    (except-last-pair! available-mutices)
    ((last-pair used-mutices) 'acquire))

  (define (release-mutex)
    (append available-mutices (last-pair used-mutices))
    (except-last-pair! used-mutices)
    ((last-pair available-mutices) 'release))

  (define (the-semaphore m)
    (cond ((eq? m 'acquire)
	   (if (mutex-available?)
	       (acquire-mutex)
	       (the-semaphore 'acquire)))
	  ((eq? m 'release)
	   (if (mutex-can-be-released?)
	       (release-mutex))))))


;; b.
(define (make-semaphore n)
  (let ((available-mutices (map false (make-list n)))
	(used-mutices '())))

  (define (mutex-available?)
    (> (length available-mutices) 0))

  (define (mutex-can-be-released?)
    (> (length used-mutices) 0))

  (define (acquire-mutex)
    (append used-mutices (last-pair available-mutices))
    (except-last-pair! available-mutices)
    (test-and-set! (last-pair used-mutices)))

  (define (release-mutex)
    (append available-mutices (last-pair used-mutices))
    (except-last-pair! used-mutices)
    (clear! (last-pair available-mutices)))

  (define (the-semaphore m)
    (cond ((eq? m 'acquire)
	   (if (mutex-available?)
	       (acquire-mutex)
	       (the-semaphore 'acquire)))
	  ((eq? m 'release)
	   (if (mutex-can-be-released?)
	       (release-mutex))))))

; 3.48
; ========================================================================
;; Instead of each mutex running at the same priority level, one can gain
;; precedence over the other.  This lets each process still run uninterrupted
;; but also allows one to pre-empt the other rather than deadlock.
;; Note you could still deadlock if two processes had the same priority level.
(define (make-account balance priority)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
	    ((eq? m 'serializer) balance-serializer)
	    ((eq? m 'priority) priority)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
	(serializer2 (account2 'serializer)))
    (if (> (account1 'priority) (account2 'priority))
	((serializer1 (serializer2 exchange)) account1 account2)
	((serializer2 (serializer1 exchange)) account2 account1))))

; 3.49
; ========================================================================
;; I'm envisioning a scenario where account1 has to aks the bank manager for
;; access to account2 and at the same time account2 is aksing the bank manager
;; for access to account1.  If these two requests take place at the same priority
;; level there'll be no way to resolve the deadlock.

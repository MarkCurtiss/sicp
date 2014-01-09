; 3.9
; ========================================================================
;; Recursive solution:
"
                        +-------------------+
 global env+----------->|factorial          |
                        |  +                |<---------------+
                        |  |                |<----+          |
                        +--|----------------+     |          |
                           |     ^                |          |
                           |     |           +----+-----+    |
                           |     |  E1+----->|n: 6      |    |
                  +--------+     |           |          |    |
                  |              |           |          |    |
                  |              |           +----------+    |
                  v              |                           +
            +------------+       |                      +----------+
            |params: n   |+------+           E2+------->|n: 5      |
            |body. . .   |                              |          |
            |            |                              |          |
            +------------+                              +----------+ ...
"

;; Iterative solution:
"
                      +------------------------------------+
 global env +-------->|factorial            fact-iter      |<--------------------------+
                      | + ^                  +    ^        |<---------------+          |
                      | | |                  |    |        |<+              |          |
                      +-|-|------------------|----|--------+ |              |          |
                        | |                  |    |          |              |          |
                        | |                  |    |          |              |          |
      +-----------------+ |       +----------+    |          |              |          |
      |                   |       |               |          |              |          |
      v                   |       v               |          +              +          |
 +-----------+            |   +----------------+  |       +--------+      +----------+ |
 |params: n  |+-----------+   |params: product |+-+  E1+->|n: 6    | E2+->|product: 1| |
 |body. . .  |                |counter, max    |          |        |      |counter: 1| |
 |           |                |body. . .       |          |        |      |max: 6    | |
 |           |                |                |          +--------+      +----------+ |
 +-----------+                +----------------+                                       +
                                                                              +----------+
                                                                         E3+->|product: 1|
                                                                              |counter: 2|
                                                                              |max: 6    |
                                                                              +----------+ ...
"

; 3.10
; ========================================================================
;; (define W1 (make-withdrawal 100))
;;
"
                +-------------------------+     +--+    params: initial-amount
global-env+---->|         make-withdrawal+----->| +---->body:
                |                         |     |--|     ((lambda (balance)
       +--------+W1                       |<-----+ |       (lambda (amount)
       |        +-------------------------+     +--+         ...
       |                     ^                             ) initial-amount)
       |            +--------+----------+
       v     E1+--->|initial-amount: 100|
     +--+           +-------------------+
  +---+ |                     ^
  |  |--|             +-------+----+
  |  | +----+  E2+--->|balance: 100|
  |  +--+   |         +------------+
  |         |               ^
  |         +---------------+
  v
params: amount
body:
  (if (>= balance amount)
    ...
"


;; (W1 50)
"

                +-------------------------+     +--+    params: initial-amount
global-env+---->|         make-withdrawal+----->| +---->body:
                |                         |     |--|     ((lambda (balance)
       +--------+W1                       |<-----+ |       (lambda (amount)
       |        +-------------------------+     +--+         ...
       |                     ^                             ) initial-amount)
       |            +--------+----------+
       v     E1+--->|initial-amount: 100|
     +--+           +-------------------+
  +---+ |                     ^           +--------------+
  |  |--|             +-------+----+      |          +---+------+
  |  | +----+  E2+--->|balance: 100|<-----+   E3+--->|amount: 50|
  |  +--+   |         +------------+                 +----------+
  |         |               ^                    (if (>= balance amount)
  |         +---------------+                      (begin (set! balance (- balance amount))
  v                                                  ...
params: amount
body:
  (if (>= balance amount)
    ...
"
;; Then:
"
                +-------------------------+     +--+    params: initial-amount
global-env+---->|         make-withdrawal+----->| +---->body:
                |                         |     |--|     ((lambda (balance)
       +--------+W1                       |<-----+ |       (lambda (amount)
       |        +-------------------------+     +--+         ...
       |                     ^                             ) initial-amount)
       |            +--------+----------+
       v     E1+--->|initial-amount: 100|
     +--+           +-------------------+
  +---+ |                     ^
  |  |--|             +-------+----+
  |  | +----+  E2+--->|balance: 50 |
  |  +--+   |         +------------+
  |         |               ^
  |         +---------------+
  v
params: amount
body:
  (if (>= balance amount)
    ...
"

;; (define W2 (make-withdrawal 100))
"
                +-------------------------+     +--+    params: initial-amount
global-env+---->|         make-withdrawal+----->| +---->body:
                |                         |     |--|     ((lambda (balance)
       +--------+W1                     W2|<-----+ |       (lambda (amount)
       |        +------------------------++     +--+         ...
       |                     ^           |^                ) initial-amount)
       |            +--------+----------+|+--------------------------+
       v     E1+--->|initial-amount: 100|+-------+              +----+--------------+
     +--+           +-------------------+        |       E3+--->|initial-amount: 100|
  +---+ |                     ^                  |              +-------------------+
  |  |--|             +-------+----+             |                       ^
  |  | +----+  E2+--->|balance: 50 |             v                +------+-----+
  |  +--+   |         +------------+            +--+       E4+--->|balance: 100|
  |         |               ^         +----------+ |              +------------+
  |         +---------------+         |         |--|                   ^
  v                                   |         | +--------------------+
params: amount <----------------------+         +--+
body:
  (if (>= balance amount)
    ...
"

;; The resultant environment differs from the non-let version of (make-withdraw)
;; in that the double-lambdas cause the resultant code objects to carry
;; around references to an environment containing "initial-amount".

; 3.11
; ========================================================================
;; (define acc (make-account 50))
"
              +------------------+          +--+    params: balance
global-env+-->|      make-account+--------->| +---->body:
              |                  |          |--|      (define (withdraw amount)
    +---------+acc               |<----------+ |        ...
    |         +------------------+          +--+      (define (deposit amount)
    |               ^                                   ...
    |               |                                 (define (dispatch m)
    v            +-----------+                          ...
  +--+    E1+--->|balance: 50|          +--+          dispatch)
+--+ |           |withdraw+------------>| +------+
| |--|           |deposit+------+       |--|     |
| | +----------->|dispatch   |<----------+ |     |
| +--+           +------+----+  |       +--+     |
|                       |  ^ ^  |                |
|                       |  | |  |                v
|                       v  | |  v            params: amount
|                    +--+  | |+--+           body:
|                +----+ |  | || +----+         (if (>= balance amount)
|                |   |--|  | ||--|   v           ...
|                |   | +---+ +-+ |   params: amount
|                |   +--+     +--+   body:
|                |                     (set! balance (+ balance amount))
+>params: m <----+                     balance)
  body:
    ((cond (eq ? m 'withdraw) withdraw)
    ...
"

;; ((acc 'deposit) 40)
"
              +------------------+          +--+    params: balance
global-env+-->|      make-account+--------->| +---->body:
              |                  |          |--|      (define (withdraw amount)
    +---------+acc               |<----------+ |        ...
    |         +------------------+          +--+      (define (deposit amount)
    |               ^                                   ...
    |               |                                 (define (dispatch m)
    v            +-----------+                          ...
  +--+    E1+--->|balance: 50|                        dispatch)
+--+ |           |withdraw   |
| |--|           |deposit<----------------------+
| | +----------->|dispatch<-------+             |
| +--+           +-----------+    |             |
|                                 |             |
|                                 |             |
|                           +-----+-----+       |
|                    E2+--->|m: 'deposit|       |
|                           +-----------+       |
|                            ((eq? m 'deposit) deposit)
|                              ...              |
|                                               |
|                                         +-----+-------+
+>params: m                        E3+--->|amount: 40   |
  body:                                   +-------------+
    ((cond (eq? m 'withdraw) withdraw)     (set! balance (+ balance amount))
      ...                                    ...
"
;; Then:
"
              +------------------+          +--+    params: balance
global-env+-->|      make-account+--------->| +---->body:
              |                  |          |--|      (define (withdraw amount)
    +---------+acc               |<----------+ |        ...
    |         +------------------+          +--+      (define (deposit amount)
    |               ^                                   ...
    |               |                                 (define (dispatch m)
    v            +-----------+                          ...
  +--+    E1+--->|balance: 90|                        dispatch)
+--+ |           |withdraw   |
| |--|           |deposit    |
| | +----------->|dispatch   |
| +--+           +-----------+
|
|
|
|
|
|
|
|
|
+>params: m
  body:
    ((cond (eq? m 'withdraw) withdraw)
      ...
"

;; ((acc 'withdraw) 60)
"
              +------------------+          +--+    params: balance
global-env+-->|      make-account+--------->| +---->body:
              |                  |          |--|      (define (withdraw amount)
    +---------+acc               |<----------+ |        ...
    |         +------------------+          +--+      (define (deposit amount)
    |               ^                                   ...
    |               |                                 (define (dispatch m)
    v            +-----------+                          ...
  +--+    E1+--->|balance: 90|                        dispatch)
+--+ |           |withdraw<----------------------+
| |--|           |deposit    |                   |
| | +----------->|dispatch<-------+              |
| +--+           +-----------+    |              |
|                                 |              |
|                          +------+-----+        |
|                   E2+--->|m: 'withdraw|        |
|                          +------------+        |
|                  ((cond (eq? m 'withdraw) withdraw)
|                    ...                   +-----+----+
|                                   E3+--->|amount: 60|
|                                          +----------+
|                                           (if (>= balance amount)
+>params: m                                   ...
  body:
    ((cond (eq? m 'withdraw) withdraw)
      ...
"
;; Then:
"
              +------------------+          +--+    params: balance
global-env+-->|      make-account+--------->| +---->body:
              |                  |          |--|      (define (withdraw amount)
    +---------+acc               |<----------+ |        ...
    |         +------------------+          +--+      (define (deposit amount)
    |               ^                                   ...
    |               |                                 (define (dispatch m)
    v            +-----------+                          ...
  +--+    E1+--->|balance: 30|                        dispatch)
+--+ |           |withdraw   |
| |--|           |deposit    |
| | +----------->|dispatch   |
| +--+           +-----------+
|
|
|
|
|
|
|
|
|
+>params: m
  body:
    ((cond (eq? m 'withdraw) withdraw)
      ...
"

;; (acc)'s local state is kept in an environment E1.
"
(define acc2 (make-account 100))
              +------------------+          +--+    params: balance
global-env+-->|      make-account+--------->| +---->body:
              |                  |          |--|      (define (withdraw amount)
    +---------+acc          acc2 |<----------+ |        ...
    |         +----------------+-+          +--+      (define (deposit amount)
    |               ^          |^                       ...
    |               |          |+---------+           (define (dispatch m)
    v            +-----------+ |       +--+--------+    ...
  +--+    E1+--->|balance: 30| |E2+--->|balance:100|  dispatch)
+--+ |           |withdraw   | |       |withdraw   |
| |--|           |deposit    | |       |deposit    |
| | +----------->|dispatch   | |       |dispatch   |
| +--+           +-----------+ |       +-----------+
|                              |            ^
|                              v            |
|                            +--+           |
|                            | +------>params: m
|                            |--|      body:
|                            | +|        ((cond (eq? m 'withdraw) withdraw)
|                            +-|+          ...
|                              |            |
|                              +------------+
+>params: m
  body:
    ((cond (eq? m 'withdraw) withdraw)
      ...
"

;; The two account objects don't share any state, not even method definitions.
;; The only thing they have in common is the same parent environment, the
;; global environment.
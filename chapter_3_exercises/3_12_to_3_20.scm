; 3.12
; ========================================================================
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

;; 1 ]=> (cdr x)
;; ;Value 12: (b)
"
    +---+  +---+
x-->|+|+-->|+|/|
    +|--+  +|--+
     |      |
     v      v
    +-+    +-+
    |a|    |b|
    +-+    +-+

    +---+  +---+
y-->|+|+-->|+|/|
    +|--+  +|--+
     |      |
     v      v
    +-+    +-+
    |c|    |d|
    +-+    +-+
"
;; 1 ]=> (cdr x)
;; ;Value 12: (b c d)
"
    +---+  +---+
x-->|+|+-->|+|+---+
    +|--+  +|--+  |
     |      |     |
     v      v     |
    +-+    +-+    |
    |a|    |b|    |
    +-+    +-+    |
+-----------------+
v   +---+  +---+
y-->|+|+-->|+|/|
    +|--+  +|--+
     |      |
     v      v
    +-+    +-+
    |c|    |d|
    +-+    +-+
"

; 3.13
; ========================================================================
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; (define z (make-cycle (list 'a 'b 'c)))
"
    +---+  +---+  +---+
z-->|+|+-->|+|+-->|+|+--+
^   +|--+  +|--+  +|--+ |
|    |      |      |    |
|    v      v      v    |
|   +-+    +-+    +-+   |
|   |a|    |b|    |c|   |
|   +-+    +-+    +-+   |
|                       |
+-----------------------+
"
;; If you call (last-pair) on this it will cause infinite recursion!

; 3.14
; ========================================================================
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;(define v (list 'a 'b 'c 'd))
"
    +---+  +---+  +---+  +---+
v-->|+|+-->|+|+-->|+|+-->|+|/|
    +|--+  +|--+  +|--+  +|--+
     |      |      |      |
     v      v      v      v
    +-+    +-+    +-+    +-+
    |a|    |b|    |c|    |d|
    +-+    +-+    +-+    +-+
"

;; (define w (mystery v))
"
    +---+
v-->|+|/|
    +|--+
     |
     v
    +-+
    |a|
    +-+

    +---+  +---+  +---+  +---+
w-->|+|+-->|+|+-->|+|+-->|+|/|
    +|--+  +|--+  +|--+  +|--+
     |      |      |      |
     v      v      v      v
    +-+    +-+    +-+    +-+
    |d|    |c|    |b|    |a|
    +-+    +-+    +-+    +-+
"

;; 1 ]=> v
;; ;Value 5: (a)

;; 1 ]=> w
;; ;Value 6: (d c b a)1

; 3.15
; ========================================================================
;; (define x (list 'a 'b))
;; (define z1 (cons x x))
"
     +---+
z1-->|+|+|
     +|-|+
      | |
      v v
     +---+  +---+
x--->|+|+-->|+|/|
     +|--+  +|--+
      |      |
      v      v
     +-+    +-+
     |a|    |b|
     +-+    +-+
"

;;(set-to-wow! z1)
"
     +---+
z1-->|+|+|
     +|-|+
      | |
      v v
     +---+  +---+
x--->|+|+-->|+|/|
     +|--+  +|--+
      |      |
      v      v
    +---+   +-+
    |wow|   |b|
    +---+   +-+
"

;; (define z2 (cons (list 'a 'b) (list 'a 'b)))
"
     +---+
z2-->|+|+----------+
     +|--+         |
      |            |
      v            |
     +---+  +---+  |
     |+|+-->|+|/|  |
     +|--+  +|--+  |
      |      |     |
      v      v     |
     +-+    +-+    |
     |a|    |b|    |
     +-+    +-+    |
      ^      ^     |
      |      |     |
     +|--+  +|--+  |
     |+|+-->|+|/|  |
     +---+  +---+  |
      ^            |
      |            |
      +------------+
"
;(set-to-wow! z2)
"
     +---+
z2-->|+|+----------+
     +|--+         |
  +---+            |
  v                |
 +---+        +---+|
 |+|+-------->|+|/||
 +|--+        +|--+|
  |            |   |
  v            v   |
+---+  +-+    +-+  |
|wow|  |a|    |b|  |
+---+  +-+    +-+  |
        ^      ^   |
        |      |   |
       +|--+  +|--+|
       |+|+-->|+|/||
       +---+  +---+|
        ^          |
        |          |
        +----------+
"
; 3.16
; ========================================================================
(define (wrong-count-pairs x)
  (if (not (pair? x))
      0
      (+ (wrong-count-pairs (car x))
         (wrong-count-pairs (cdr x))
         1)))
"
     +---+  +---+  +---+
x+-->|+|+-->|+|+-->|+|/|
     +|--+  +|--+  +|--+
      |      |      |
      v      v      v
     +-+    +-+    +-+
     |a|    |b|    |c|
     +-+    +-+    +-+
"
;; 1 ]=> (define x '(a b c))
;; ;Value: x
;; 1 ]=> (wrong-count-pairs x)
;; ;Value: 3
"
     +---+  +---+  +---+
x+-->|+|+-->|+|+-->|+|/|
     +|--+  +|--+  +|--+
      |      |     ^|
      |      v   +-+v
      |     +-+  | +-+
      |     |b|  | |c|
      |     +-+  | +-+
      |          |
      |          |
      |          |
      +----------+
"
;; 1 ]=> (define x (list 'a 'b 'c))
;; ;Value: x
;; 1 ]=> (set-car! x (cddr x))
;; ;Unspecified return value
;; 1 ]=> (wrong-count-pairs x)
;; ;Value: 4
"
     +---+  +---+  +---+
x+-->|+|+-->|+|+-->|+|/|
     +|--+  +|--+  +|--+
      | ^    | ^    |
      | |    | |    v
      +-+    +-+   +-+
                   |c|
                   +-+
"
;; 1 ]=> (define x (list 'a 'b 'c))
;; ;Value: x
;; 1 ]=> (set-car! (cdr x) (cddr x))
;; ;Unspecified return value
;; 1 ]=> (set-car! x (cdr x))
;; ;Unspecified return value
;; 1 ]=> (wrong-count-pairs x)
;; ;Value: 7
"
     +---+  +---+  +---+
x+-->|+|+-->|+|+-->|+|/|
 +-->+|--+  +|--+  +|--+
 |    |      |      |
 |    v      v      |
 |   +-+    +-+     |
 |   |a|    |b|     |
 |   +-+    +-+     |
 |                  |
 +------------------+
"
;; 1 ]=> (define x (list 'a 'b 'c))
;; ;Value: x
;; 1 ]=> (set-car! (cddr x) x)
;; ;Unspecified return value
;; 1 ]=> (wrong-count-pairs x)
;; ;Aborting!: maximum recursion depth exceeded

; 3.17
; ========================================================================
(define (count-pairs x)
  (define (populate-hash-table x seen-items)
    (if (null? x)
	seen-items
	(begin
	  (hash-table/put! seen-items x true)
	  (populate-hash-table (cdr x) seen-items)))
    )

  (define hash (populate-hash-table x (make-strong-eq-hash-table)))
  (hash-table/count hash))

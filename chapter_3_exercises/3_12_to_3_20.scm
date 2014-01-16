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

; 3.18
; ========================================================================
(define (is-cyclical? x)
  (define (find-cycles x seen-pairs)
    (cond ((null? x) false)
	  ((hash-table/get seen-pairs x false) true)
	  (else
	   (hash-table/put! seen-pairs x true)
	   (find-cycles (cdr x) seen-pairs))))

  (find-cycles x (make-strong-eq-hash-table)))

; 3.19
; ========================================================================
(define (is-cyclical? x)
  (define (find-cycles current-node current-index last-visited-node last-index)
    (cond ((= current-index last-index) false)
	  ((eq? current-node last-visited-node) true)
	  (else (find-cycles (cdr current-node)
			     (+ 1 current-index)
			     last-visited-node
			     last-index))))

  (define (iter current i)
    (cond ((null? current) false)
	  ((find-cycles x 0 current i) true)
	  (else (iter (cdr current) (+ 1 i)))))

  (iter x 0)
  )

; 3.20
; ========================================================================
;; (define x (cons 1 2))
"
              +------------------------+          +--+     params: x, y
global env+-->|                   cons+---------->| +----->body:
              |                        |          +--+      (define (set-x! v)
              |                        |<----------+ |        ...
        +-----+x                       |          +--+      (define (set-y! v)
        |     +------------------------+                      ...
        |                   ^                               (define (dispatch m)
        |                   |                                 ...
        v            +------+------+                        dispatch)
       +--+    E1+-->|x: 1, y: 2   |    +--+
+-------+ |          |set-x!+---------->| +-->params: v
|      +--+          |set-y!+---------+ +--+  body:
|      | +---------->|dispatch+    |<-|--+ |    (set! x v)
|      +--+          +--------|----+  | +--+
|                             |   ^   |
|                             |   |   |
|                             |   |   |
|                             |   |   v
|                             |   |  +--+
|                             |   |  | +-->params: v
|                             |   |  +--+  body:
+>params: m <-----------------+   +---+ |    (set! y v)
  body:                              +--+
    (cond ((eq m 'car) x)
    ...
"
;;(define z (cons x x))
"
              +------------------------+          +--+     params: x, y
global env+-->|                   cons+---------->| +----->body:
              |                        |          +--+      (define (set-x! v)
              |                        |<----------+ |        ...
        +-----+x                      z+-------+  +--+      (define (set-y! v)
        |     +------------------------+       |              ...
        |                   ^         ^        |            (define (dispatch m)
        |                   |         |        |              ...
        v            +------+------+  |        |            dispatch)
       +--+    E1+-->|x: 1, y: 2   |  |        |
+-------+ |          |set-x!       |  +-------------------------+
|      +--+          |set-y!       |           |                |
|      | +---------->|dispatch+    |           v         +------+------+
|      +--+          +--------|----+          +--+  E2+->|x: x, y: x   |
|                             |            +---+ |       |set-x!       |
|                             |            |  +--+       |set-y!       |
|                             |            |  | +------->|dispatch+    |
|                             |            |  +--+      ++--------|----+
|                             |            |                      |
|                             |            |                      |
|                             |            |                      |
+>params: m <-----------------+            v                      |
  body:                                    params: m <------------+
    (cond ((eq m 'car) x)                  body:
    ...                                      (cond ((eq m 'car) x)
                                             ...
"
;;(set-car! z (cdr z) 17)
"
              +------------------------+
global env+-->|                        |
              |                        |
              |                        |<-----------------------------+
        +-----+x                      z+-------+                      +
        |     +------------------------+       |                   +----+
        |                   ^         ^        |             E3+-->|z: z|<----------------+
        |                   |         |        |                   +----+                 |
        v            +------+------+  |        |                    (z 'cdr)              |
       +--+    E1+-->|x: 1, y: 2   |  |        |                                          |
+-------+ |          |set-x!       |  +-------------------------+                  +------+------+
|      +--+          |set-y!       |           |                |            E4+-->|z: (cdr z)   |
|      | +---------->|dispatch+    |           v         +------+------+           |new-value: 17|
|      +--+          +--------|----+          +--+  E2+->|x: x, y: x   |           +-------------+
|                             |            +---+ |       |set-x!       |   ((z 'set-car!) new-value)
|                             |            |  +--+       |set-y!       |     z)
|                             |            |  | +------->|dispatch+    |
|                             |            |  +--+      ++--------|----+
|                             |            |                      |
|                             |            |                      |
|                             |            |                      |
+>params: m <-----------------+            v                      |
  body:                                    params: m <------------+
    (cond ((eq m 'car) x)                  body:
    ...                                      (cond ((eq m 'car) x)
                                             ...
"

;;(car x)
"
              +------------------------+
global env+-->|                        |
              |                        |<------------------------+
              |                        |                       +-+--+
        +-----+x                      z+-------+        E3+--->|z: x|
        |     +------------------------+       |               +----+
        |                   ^         ^        |                (z 'car)
        |                   |         |        |
        v            +------+------+  |        |
       +--+    E1+-->|x: 17, y: 2  |  |        |
+-------+ |          |set-x!       |  +-------------------------+
|      +--+          |set-y!       |           |                |
|      | +---------->|dispatch+    |           v         +------+------+
|      +--+          +--------|----+          +--+  E2+->|x: x, y: x   |
|                             |            +---+ |       |set-x!       |
|                             |            |  +--+       |set-y!       |
|                             |            |  | +------->|dispatch+    |
|                             |            |  +--+      ++--------|----+
|                             |            |                      |
|                             |            |                      |
|                             |            |                      |
+>params: m <-----------------+            v                      |
  body:                                    params: m <------------+
    (cond ((eq m 'car) x)                  body:
    ...                                      (cond ((eq m 'car) x)
                                             ...
"
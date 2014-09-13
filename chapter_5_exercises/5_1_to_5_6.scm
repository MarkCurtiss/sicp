; 5.1
; ========================================================================
;; Here is the data flow diagram.  Note that ASCII art prevents me from
;; doing trapezoids or circles with x's in them:
"
               +--+-----------------------------+
-+-+      +----+--+                             |   +-------+
|> +------+counter+---+                         |   |product<+
---+      +-------+   |                         |   +--+----++
 |                |   |                         |      |     |
 |                |   +-----+                   |      |     |
+++               |         |                   |      |     |
|n|      +-+    +-v---+     |                  +++     |     |
+-+      |1+----+ plus|     |counter<-cp1      |*+-----+     |
         +-+    +-+---+     |                  +++           |
                  |         |                   |            |product<-pc
                  |         |                   |            |
                +-v-+---++--+                 +-v+           |
                |cp1|cp1<-r                   |pc|pc<-r      |
                +---+                         ++-+           |
                                               |             |
                                               |             |
                                               +-------------+
"

;; Here is the control diagram.
"
     start
       +
       |
       |
       |
      +--+  yes
+---> |> +-------->done
|     +--+
|      |
|      |
|      |
|    +-v++--+
|    |pc<-r |
|    |      |
|    +-+----+
|      |
|    +-v-++-+
|    |cp1<-r|
|    |      |
|    +-+----+
|      |
|    +-v-----++--+
|    |product<-pc|
|    |           |
|    +-+---------+
|      |
|    +-v-----++---+
|    |counter<-cp1|
|    |            |
|    +---+--------+
|        |
|        |
|        |
+--------+
"

; 5.2
; ========================================================================
(controller
 (assign product 1)
 (assign counter 1)

 test-counter
   (test (op >) (reg counter) (reg n))
   (branch (label factorial-done))
   (assign pc (op *) (reg product) (reg counter))
   (assign cp1 (op +) (reg counter) (constant 1))
   (assign product (reg pc))
   (assign counter (reg cp1))
   (goto (label test-counter))
 factorial-done)

; 5.3
; ========================================================================
;; Assuming primitive operations.
(controller
 (assign guess 1.0)

 test-guess
   (test (op good-enough?) (reg guess))
   (branch (label sqrt-done))
   (assign guess (op improve) (reg guess))
   (goto (label test-guess))
 sqrt-done)

;; Expanding the primitive operations.
(controller
 (assign guess 1.0)
 (assign x (op read))

 test-guess
;; good-enough?
   (assign sqrguess (op square) (reg guess))
   (assign sqrminusx (op -) (reg sqrguess) (reg x))
   (assign absresult (op abs) (reg sqrminusx))
   (test (op <) (reg absresult) (constant 0.001))
   (branch (label sqrt-done))
;; improve
   (assign xdivguess (op /) (reg x) (reg guess))
   (assign avgguess (op avg) (reg xdivguess) (reg guess))
   (assign guess (reg avgguess))
   (goto (label test-guess))
 sqrt-done)

;; I drew diagrams for these but they are drawn in a pen on a notebook
;; in front of me and I don't feel like drawing them in ASCII art.
;; I guess I can show my study-group colleagues the drawing when we
;; meet up.

; 5.4
; ========================================================================
;; a.
;; (define (expt b n)
;;   (if (= n 0)
;;       1
;;       (* b (expt b (- n 1)))))

(controller
 (assign b (op read))
 (assign n (op read))
 (assign continue (label expt-done))

 expt-loop
   (test (op =) (reg n) (const 0))
   (branch (label base-case))
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-expt))
   (goto (label expt-loop))
 after-expt
   (restore n)
   (restore continue)
   (assign val (op *) (reg b) (reg val))
 base-case
   (assign val (const 1))
   (goto (reg continue))
 expt-done)

;; b.
;; (define (expt b n)
;;   (define (expt-iter counter product)
;;     (if (= counter 0)
;;         product
;;         (expt-iter (- counter 1) (* b product))))
;;   (expt-iter n 1))

(controller
 (assign b (op read))
 (assign n (op read))
 (assign product 1)

 test-counter
   (test (op =) (reg n) (const 0))
   (branch (label expt-done))
   (assign n (op -) (reg n) (const 1))
   (assign product (op *) (reg b) (reg product))
   (goto (label test-counter))
 expt-done)

;; I did not draw diagrams for these.  It was easier to just write the
;; controller instruction sequence.

; 5.5
; ========================================================================
This is a stepthrough of what happens when you call (factorial 2):

before we start fact-loop:
  continue: label fact-done
  n: 2
  val: ()
  stack: ()

after we've executed fact-loop but before we (goto (label fact-loop)):
  continue: label after-fact
  n: 1
  val: ()
  stack:
    continue: label fact-done
    n: 2

after we (goto (label fact-loop)) and we (test) n against 1:
  continue: label after-fact
  n: 1
  val: ()
  stack:
    continue: label fact-done
    n: 2

since n == 1, we (branch) to label base-case and assign a val:
  continue: label after-fact
  n: 1
  val: 1
  stack:
    continue: label fact-done
    n: 2

then we (goto (reg continue)) which resolves to after-fact.  we also restore n and continue:
  continue: fact-done
  n: 2
  val: 1
  stack: ()

now multiply n*val and assign to val, then (goto (reg continue)) which dumps us at fact-done.
  continue: fact-done
  n: 2
  val: 2
  stack: ()


This is a stepthrough of what happens if you call (fib 3)

before we start fib-loop:
  continue: label fib-done
  n: 3
  val: ()
  stack: ()

our test against n < 2 fails and we prepare to (goto fib-loop):
  continue: label after-fib-n-1
  n: 2
  val: ()
  stack:
    continue: label fib-done
    n: 3

our test against n < 2 fails and we prepare to (goto fib-loop) again:
  continue: label after-fib-n-1
  n: 1
  val: ()
  stack:
    continue: label after-fib-n-1, label fib-done
    n: 2, 3

in (fib-loop) our test against n < 2 passes and we (branch) to immediate-answer and assign a val:
  continue: label after-fib-n-1
  n: 1
  val: 1
  stack:
    continue: label after-fib-n-1, label fib-done
    n: 2, 3

we then (goto after-fib-n-1) and restore n and continue:
  continue: label after-fib-n-1
  n: 2
  val: 1
  stack:
    continue: label fib-done
    n: 3

we then assign and save continue, n, and val before we (goto fib-loop):
  continue: label after-fib-n-2
  n: 0
  val: 1
  stack:
    continue: label after-fib-n-1, label fib-done
    n: 3
    val: 1

in fib-loop, test n < 2 succeeds and we (goto immediate-answer) and assign val to n:
  continue: label after-fib-n-2
  n: 0
  val: 0
  stack:
    continue: label after-fib-n-1, label fib-done
    n: 3
    val: 1

then we (goto after-fib-n-2) and assign n to val:
  continue: label after-fib-n-2
  n: 0
  val: 0
  stack:
    continue: label after-fib-n-1, label fib-done
    n: 3
    val: 1

we restore val and continue:
  continue: label after-fib-n-1
  n: 0
  val: 1
  stack:
    continue: label fib-done
    n: 3

then we assign to val (+ val n) and (goto continue)
  continue: label after-fib-n-1
  n: 0
  val: 1
  stack:
    continue: label fib-done
    n: 3

in after-fib-n-1 we restore n and continue:
  continue: label fib-done
  n: 3
  val: 1
  stack: ()

we assign and save continue, n, and val:
  continue: label after-fib-n-2
  n: 1
  val: 1
  stack:
    continue: label fib-done
    val: 1

we (goto fib-loop).  our test of n < 2 passes so we branch to immediate-answer and assign val:
  continue: label after-fib-n-2
  n: 1
  val: 1
  stack:
    continue: label fib-done
    val: 1

we then (goto continue).  in (after-fib-n-2) we assign val to n:
  continue: label after-fib-n-2
  n: 1
  val: 1
  stack:
    continue: label fib-done
    val: 1

we then restore val and continue:
  continue: label fib-done
  n: 1
  val: 1
  stack: ()

we assign val (+ val n):
  continue: label fib-done
  n: 1
  val: 2
  stack: ()

and jump to (reg continue) which lands us at fib-done with a value of 2.

; 5.6
; ========================================================================
In after-fib-n-1, we (restore) continue only to (save) it afterwards
with no intermediate modifications.

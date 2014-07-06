; 4.25
; ========================================================================
; When you try and evaluate (factorial 5) you'll recurse indefinitely as
; the second argument to (unless) is another call to (factorial).
; This would work with a lazily evaluated language, however.

; 4.26
; ========================================================================
;; If (unless) were a special form you'd be unable to pass it to procedures.
;; So if you were trying to call map and pass it a tri-part predicate you
;; couldn't pass it (unless) which would be a shame.  I can't think of a
;; situation where you wouldn't want (unless) as a procedure!  I am having
;; a hard time supporting Ben Bitdiddle's argument.

;; Okay I guess the only argument I can make for Ben Bitdiddle is that implementing
;; (unless) as a special form lets you use it in applicative-order languages.  If
;; you went with Alyssa's procedure solution you couldn't ever use it.

;; (But also I think (unless) kind of sucks and you should use it sparingly).

;; Also I think it is funny that this book is promoting developer arguments.

(define put 2d-put!)
(define get 2d-get)

(define (install-unless-package)
  (define (unless-predicate exp) (cadr exp))
  (define (unless-consequent exp) (caddr exp))
  (define (unless-alternative exp) (cadddr exp))

  (define (eval-unless exp env)
    (if (true? (eval (unless-predicate exp) env))
	(eval (unless-consequent exp) env)
	(eval (unless-alternative exp) env)))

  (put 'eval 'unless eval-unless)

  'unless-package-installed)

(install-unless-package)

; 4.27
; ========================================================================
(load "book_code/ch4-leval.scm")

;; (define the-global-environment (setup-environment))
;; (driver-loop)

;; (define count 0)
;; (define (id x)
;;   (set! count (+ count 1))
;;   x)

;; ;;; L-Eval input:
;; (define w (id (id 10)))
;; ;;; L-Eval value:
;; ok

;; ;;; L-Eval input:
;; count
;; ;;; L-Eval value:
;; 1
;; We evaluated the first application of (id) when we (define)'d w so our count
;; is now (0+1 ==) 1.

;; ;;; L-Eval input:
;; w
;; ;;; L-Eval value:
;; 10
;; This applies (id) again which has a cool side effect. . .

;; ;;; L-Eval input:
;; count
;; ;;; L-Eval value:
;; 2
;; You see that now count has been incremented a second time.  It is 2.

; 4.28
; ========================================================================
;; If you had an operator that had been thunk'ed, you would need to force its
;; value before you could dispatch on its type.

; 4.29
; ========================================================================
;; (factorial) is the classic example as you'll be recomputing (factorial)
;; invocations over and over again.

(define the-global-environment (setup-environment))
(driver-loop)

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)
(define (square x)
  (* x x))

;; With memoization.
;;; L-Eval input:
;; (define (square x)
;;   (* x x))
;; ;;; L-Eval value:
;; ok
;; ;;; L-Eval input:
;; (square (id 10))
;; ;;; L-Eval value:
;; 100
;; ;;; L-Eval input:
;; count
;; ;;; L-Eval value:
;; 1

;; Without memoization.
;; (load "book_code/ch4-nonmemoizing-leval.scm")
;; (define the-global-environment (setup-environment))
;; (driver-loop)
;; (define count 0)
;; (define (id x)
;;   (set! count (+ count 1))
;;   x)
;; (define (square x)
;;   (* x x))
;; ;;; L-Eval input:
;; (square (id 10))

;; ;;; L-Eval value:
;; 100

;; ;;; L-Eval input:
;; count

;; ;;; L-Eval value:
;; 2

; 4.30
; ========================================================================
; a.
; Every step of (for-each) is an (application?) which means it'll get
; get passed through to (apply).  (apply) forces the actual value of its
; arguments.  Thus every element in the list gets Ben's lambda applied to it.

; b.
; With the original (eval-sequence):
;; (load "book_code/ch4-leval.scm")
;; (define the-global-environment (setup-environment))
;; (driver-loop)

;; (define (p1 x)
;;   (set! x (cons x '(2)))
;;   x)

;; (define (p2 x)
;;   (define (p e)
;;     e
;;     x)
;;   (p (set! x (cons x '(2)))))

;; (p1 1)
;; (p2 1)

;; ;;; L-Eval input:
;; (define (p1 x)
;;   (set! x (cons x '(2)))
;;   x)

;; ;;; L-Eval value:
;; ok

;; ;;; L-Eval input:
;; (define (p2 x)
;;   (define (p e)
;;     e
;;     x)
;;   (p (set! x (cons x '(2)))))

;; ;;; L-Eval value:
;; ok

;; ;;; L-Eval input:
;; (p1 1)

;; ;;; L-Eval value:
;; (1 2)

;; ;;; L-Eval input:
;; (p2 1)

;; ;;; L-Eval value:
;; 1

; With Cy D. Fect's (eval-sequence):
;; (load "book_code/ch4-cydeffect-leval.scm")
;; (define the-global-environment (setup-environment))
;; (driver-loop)

;; (define (p1 x)
;;   (set! x (cons x '(2)))
;;   x)

;; (define (p2 x)
;;   (define (p e)
;;     e
;;     x)
;;   (p (set! x (cons x '(2)))))

;; (p1 1)
;; (p2 1)

;; ;;; L-Eval input:
;; (define (p1 x)
;;   (set! x (cons x '(2)))
;;   x)

;; ;;; L-Eval value:
;; ok

;; ;;; L-Eval input:
;; (define (p2 x)
;;   (define (p e)
;;     e
;;     x)
;;   (p (set! x (cons x '(2)))))

;; ;;; L-Eval value:
;; ok

;; ;;; L-Eval input:
;; (p1 1)

;; ;;; L-Eval value:
;; (1 2)

;; ;;; L-Eval input:
;; (p2 1)

;; ;;; L-Eval value:
;; (1 2)

;; ;;; L-Eval input:

; c.
; There's no difference between the (actual-value) and the (apply)
; of Ben's function because (apply) calls (actual-value).

; d.
; I prefer the approach in the text.  It is more consistently strange.
; Cy D. Effect's approach suddenly works differently than the rest of the
; language and I dislike the thought of programming in a language where
; sequences have special behavior.

; 4.31
; ========================================================================
;; My code is in book_code/upwards-compatible-leval.scm
;; (define the-global-environment (setup-environment))
;; (driver-loop)
;; (define (owls a (b lazy) c (d lazy-memo))
;;   (+ a b c d))
;; (owls 3 4 5 6)

;; I think the code I have should work.  But for some reason it doesn't know
;; about (display) or (+) or (pp) or any other function I try to call inside
;; of (owls).  Trying to debug why inside of a scheme sub-interpreter is really
;; difficult.

; 4.32
; ========================================================================
;; (define (cons x y)
;;   (lambda (m) (m x y)))
;; (define (car z)
;;   (z (lambda (p q) p)))
;; (define (cdr z)
;;   (z (lambda (p q) q)))

; This formulation of (cons) lets us put infinite streams in both the car
; and cdr of a cons.  So you could make a stream where the car was just ones
; and the cdr was integers.

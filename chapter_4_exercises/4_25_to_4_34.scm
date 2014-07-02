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

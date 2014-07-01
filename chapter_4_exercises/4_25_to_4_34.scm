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

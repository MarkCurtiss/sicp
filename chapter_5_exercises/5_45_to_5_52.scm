; 5.45
; ========================================================================
(load "book_code/load-eceval-compiler.scm")

(compile-and-go
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

;; (total-pushes = 0 maximum-depth = 0)
;;  EC-Eval value:
;; ok

;;  EC-Eval input:
;; (factorial 1)

;; (total-pushes = 7 maximum-depth = 3)
;;  EC-Eval value:
;; 1

;;  EC-Eval input:
;; (factorial 2)

;; (total-pushes = 13 maximum-depth = 5)
;;  EC-Eval value:
;; 2

;;  EC-Eval input:
;; (factorial 3)

;; (total-pushes = 19 maximum-depth = 8)
;;  EC-Eval value:
;; 6

;; EC-Eval input:
;; (factorial 4)

;; (total-pushes = 25 maximum-depth = 11)
;; EC-Eval value:
;; 24

;; a.
;; Compiled version
;; Num pushes: 6n + 1
;; Max depth: 3n - 1 (for n > 2)

;; Interpreted version
;; Num pushes: 16 * (2n - 1)
;; Max depth: 8 + 5(n - 1)

;; Special-purpose factorial machine version
;; Num pushes: 2*n - 2
;; Max depth: 2*n - 2

;; Compiled vs interpreted
;; For the compiled (factorial 1000)
;; (total-pushes = 6001 maximum-depth = 2999)
;; For the interpreted (factorial 1000)
;; (total-pushes = 31984 maximum-depth = 5003)
;; The total pushes for the compiled version appear to be 1/5th that
;; of the interpreted version.
;; The max depth for the compiled version appear to be 3/5th that
;; of the interpreted version.

;; Special-purpose vs interpreted
;; For the special-purpose (factorial 1000)
;; (total-pushes = 1998 maximum-depth = 1998)
;; For the interpreted (factorial 1000)
;; (total-pushes = 31984 maximum-depth = 5003)
;; The total pushes for the special-purpose machine appear to be 1/16th that
;; of the interpreted version. (!)
;; The max depth for the special-purpose machine appear to be 2/5th that
;; of the interpreted version.

;; The special-purpose version is much more efficient than the compiled version
;; and dramatically embarrasingly more efficient than the interpreted version.

;; b.
;; Looking at the compiled version's "machine-language" output, a big obvious
;; win that I see would be using open-coded primitives.  The machine has to do
;; lots of saves and restores to call (-) and (*).

;; Jeez also it seems silly that you are constantly testing all of your
;; procedures and then branching based on whether they are primitive or compiled.
;; If you maintained a compile-time environment then in (compile-application)
;; you could first check if your procedure exists in the compile-time environment.
;; Then you could dispatch to (compile-procedure-call) and optimize out the
;; primitive vs compiled checks, instead generating the appropriate code directly.
;; This would save you quite a bit of testing and branching at runtime.
;; That wouldn't affect your stack depth but it'd save you some instructions.

; 5.46
; ========================================================================
(load "book_code/load-eceval-compiler.scm")

(compile-and-go
 '(define (fib n)
    (if (< n 2)
	n
	(+ (fib (- n 1)) (fib (- n 2)))))
 )

;; Interpreted
;; (fib 25)
;; (total-pushes = 6,797,968 maximum-depth = 128)

;; Compiled
;; (fib 25)
;; (total-pushes = 1,213,927 maximum-depth = 74)

;; Special-purpose
;; (fib 25)
;; (total-pushes = 485,568 maximum-depth = 48)

;; Once again, the interpreted version is comically inefficient.
;; The special-purpose version, meanwhile has an order of magnitude
;; less pushes than the other two!  Even the compiled version performs
;; over a million stack operations.  It is still 6x more efficient than
;; the interpreted version, though.
;; The maximum stack depth doesn't seem that interesting by comparison.

; 5.47
; ========================================================================
;; I added my changes to interpreted-procedures-compiler.scm
(load "book_code/load-eceval-compiler.scm")
(load "interpreted-procedures-compiler.scm")

(compile-and-go
 '(define (f x)
  (* 2 (g x)))
 )

;; 1 ]=> (compile-and-go
;;  '(define (f x)
;;   (* 2 (g x)))
;;  )

;; (total-pushes = 0 maximum-depth = 0)
;; EC-Eval value:
;; ok

;; EC-Eval input:
;; (define (g x) (+ 3 x))

;; (total-pushes = 3 maximum-depth = 3)
;; EC-Eval value:
;; ok

;; EC-Eval input:
;; (f 2)

;; (total-pushes = 16 maximum-depth = 7)
;; EC-Eval value:
;; 10

; 5.48
; ========================================================================
(load "book_code/load-eceval-compiler.scm")
(start-eceval)

;; See my changes to book_code/ch5-eceval-compiler.scm.  I added two new
;; methods, (compile-and-run?) and (compile-into-machine) and added
;; a test for it in eval-dispatch.

(compile-and-run
 (define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

;; 1 ]=> (start-eceval)
;; EC-Eval input:
;; (compile-and-run
;;  (define (factorial n)
;;     (if (= n 1)
;;         1
;;         (* (factorial (- n 1)) n))))

;; (total-pushes = 3 maximum-depth = 3)
;; EC-Eval value:
;; ok

;; EC-Eval input:
;; (factorial 5)

;; (total-pushes = 144 maximum-depth = 28)
;; EC-Eval value:
;; 120

; 5.49
; ========================================================================
(load "rcel-register-machine.scm")
(start-rcel)

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;; 1 ]=> (start-rcel)

;; EC-Eval input:
;;  (define (factorial n)
;;     (if (= n 1)
;;         1
;;         (* (factorial (- n 1)) n)))

;; (total-pushes = 0 maximum-depth = 0)
;; EC-Eval value:
;; ok

;; EC-Eval input:
;; (factorial 5)

;; (total-pushes = 26 maximum-depth = 14)
;; EC-Eval value:
;; 120

; 5.50
; ========================================================================
(load "book_code/original-ch5-regsim.scm")
(load "compile-mcevaluator.scm")

(define mceval-instructions (compile-metacircular-evaluator))

(define compiled-machine
  (make-machine
   '(exp env val proc argl continue unev)
   '()
   mceval-instructions))

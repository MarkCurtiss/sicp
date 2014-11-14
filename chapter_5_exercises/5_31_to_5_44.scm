; 5.31
; ========================================================================
;; 1. saves and restores the env register around the evaluation of the operator
;; 2. saves and restores env around the evaluation of each operand (except the final one)
;; 3. saves and restores argl around the evaluation of each operand
;; 4. saves and restores proc around the evaluation of the operand sequence

(f 'x 'y)
;; You could optimize out 1, 2, 3, 4

((f) 'x 'y)
;; You could optimize out 1, 2, 3, 4

(f (g 'x) y)
;; You could optimize out 1, 2

(f (g 'x) 'y)
;; You could optimize out 1, 2

; 5.32
; ========================================================================
;; a.

;; Put the following in ch5-eceval.scm

;; +(define (application-of-symbol? exp)
;; +  (and (application? exp)
;; +       (symbol? (operator exp))))

;;     (list 'application? application?)
;; +   (list 'application-of-symbol? application-of-symbol?)

;;    (branch (label ev-begin))
;; +  (test (op application-of-symbol?) (reg exp))
;; +  (branch (label ev-symbol-application))

;; +ev-symbol-application
;; +  (save continue)
;; +  (assign unev (op operands) (reg exp))
;; +  (save unev)
;; +  (assign exp (op operator) (reg exp))
;; +  (assign continue (label ev-appl-did-operator))
;; +  (goto (label eval-dispatch))

;; Notice that ev-symbol-application doesn't save (env) before jumping
;; to eval-dispatch.

;; b.
;; I am against this idea!  It would make the evaluator much more complex.
;; Also you'd have scads of code in the evaluator that was related solely
;; to performance optimizations rather than the syntax and semantics of
;; your language.

; 5.33
; ========================================================================
(load "book_code/ch5-compiler.scm")

(pp
 (compile
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n)))
 'val
 'next)
 )

((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry87) (reg env))
  (goto (label after-lambda86))
entry87
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch102))
compiled-branch101
  (assign continue (label after-call100))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch102
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call100
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch89))
true-branch90
  (assign val (const 1))
  (goto (reg continue))
false-branch89
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch93))
compiled-branch92
  (assign continue (label after-call91))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch93
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call91
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch96))
compiled-branch95
  (assign continue (label after-call94))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch96
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call94
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch99))
compiled-branch98
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch99
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call97
after-if88
after-lambda86
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))))

(pp
 (compile
  '(define (factorial-alt n)
     (if (= n 1)
	 1
	 (* n (factorial-alt (- n 1)))))
  'val
  'next)
 )

((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry104) (reg env))
  (goto (label after-lambda103))
entry104
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch119))
compiled-branch118
  (assign continue (label after-call117))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch119
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call117
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch106))
true-branch107
  (assign val (const 1))
  (goto (reg continue))
false-branch106
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch110))
compiled-branch109
  (assign continue (label after-call108))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch110
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call108
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch113))
compiled-branch112
  (assign continue (label after-call111))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch113
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call111
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch116))
compiled-branch115
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch116
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call114
after-if105
after-lambda103
  (perform (op define-variable!) (const factorial-alt) (reg val) (reg env))
  (assign val (const ok))))

;; The factorial-alt version has less statements since it doesn't require
;; as many intermediate steps - it simply multiplies * by the rest of the
;; arguments.
;; They have the same number of (save) statements but (factorial-alt) compiles
;; down to 11 statements while the original (factorial) compiles down to 14
;; statements.  So I'd expect (factorial-alt) to be more efficient.

; 5.34
; ========================================================================
(pp
 (compile '(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
	  'val
	  'next)
 )

((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry2) (reg env)) ;; compile entry2 into (factorial)
  (goto (label after-lambda1))
entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry7) (reg env)) ;; compile entry7 into (iter)
  (goto (label after-lambda6))
entry7  ;; (iter)
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const >) (reg env))  ;; assemble (> counter n) into proc and argl
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc)) ;; > is primitive so we'll jump to primitive-branch22
  (branch (label primitive-branch22))
compiled-branch21
  (assign continue (label after-call20))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch22
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ;; Assign the result of (< counter n) to val and fall through
after-call20
  (restore env)
  (restore continue)
  (test (op false?) (reg val))  ; Is (< counter n) false?
  (branch (label false-branch9))
true-branch10  ;; This is where we return from (iter)
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))
false-branch9 ;; If (< counter) is false, we have to compute (iter (* counter product) (+ 1 counter))
  (assign proc (op lookup-variable-value) (const iter) (reg env)) ;; set up to compute (iter (+ 1 counter))
  (save continue) ;; Gets restored in after-call11
  (save proc) ;; Gets restored in after-call11
  (save env) ;; Gets restored in after-call14
  (assign proc (op lookup-variable-value) (const +) (reg env)) ;; set up to compute (+ 1 counter)
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc)) ;; This will be true
  (branch (label primitive-branch16))
compiled-branch15
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch16
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ;; Apply (+ 1 counter) and fall through
after-call14
  (assign argl (op list) (reg val)) ;; Assign (+ 1 counter) to val
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const *) (reg env)) ;; Set up to compute (* counter product)
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc)) ;; This will be true
  (branch (label primitive-branch13))
compiled-branch12
  (assign continue (label after-call11))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch13
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ;; Assign (* counter product) to val
after-call11
  (restore argl) ;; argl now contains the result of (+ 1 counter)
  (assign argl (op cons) (reg val) (reg argl)) ;; argl now contains the result of (* counter product) and (+ 1 counter product)
  (restore proc) ;; proc now contains iter
  (restore continue) ;; continue now contains whatever it initially did when we entered the procedure
  (test (op primitive-procedure?) (reg proc)) ;; This will be false
  (branch (label primitive-branch19))
compiled-branch18
  (assign val (op compiled-procedure-entry) (reg proc)) ;;Run the code for (iter) and return to whatever continue points at
  (goto (reg val))
primitive-branch19
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call17
after-if8
after-lambda6 ;; This defines (iter) and calls (iter 1 1)
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc)) ;; This will be false
  (branch (label primitive-branch5))
compiled-branch4
  (assign val (op compiled-procedure-entry) (reg proc)) ;; Run (iter) and assign the result to val
  (goto (reg val)) ;; This doesn't actually get executed
primitive-branch5
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call3
after-lambda1
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))))

;; As you'll see in false-branch9, every (save) is matched by a (restore) before
;; iter is called again in compiled-branch18.  This means our stack stays constant
;; with n, as we don't have to save any state from one procedure invocation to the next.

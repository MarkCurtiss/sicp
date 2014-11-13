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

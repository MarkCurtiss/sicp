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

; 5.35
; ========================================================================
;; I think it is
(pp (compile
	   '(define (f x)
	      (+ x
		 (g (+ x 2))))
	   'val
	   'next))
;; I compared my output to the output in the book and they are the same !


; 5.36
; ========================================================================
;; Right to left.
;; This happens in (construct-arglist) where it (reverses) the operands
;; and then builds up the argument list thusly:
;; (op cons) (reg val) (reg argl)

;; I'm not going to modify the compiler!  In order to change the evaluation
;; to left-to-right I could remove the (reverse) call in construct-arglist.
;; It would make construct-arglist more efficient.

; 5.37
; ========================================================================
;; I commented out the following in (preserving)
;; (if (and (needs-register? seq2 first-reg)
;;          (modifies-register? seq1 first-reg))

1 ]=> (pp (compile '(+ x 2) 'val 'next))
((env continue)
 (env proc argl continue val)
 (
  (save continue)
  (save env)
  (save continue)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (restore continue)
  (restore env)
  (restore continue)
  (save continue)
  (save proc)
  (save env)
  (save continue)
  (assign val (const 2))
  (restore continue)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (save continue)
  (assign val (op lookup-variable-value) (const x) (reg env))
  (restore continue)
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch14))
  compiled-branch13
  (assign continue (label after-call12))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch14
  (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  after-call12))

;; All of those saves and restores are useless!
;; Note how the smart version of (preserving) doesn't generate any of them
1 ]=> (pp (compile '(+ x 2) 'val 'next))
((env)
 (env proc argl continue val)
 (
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 2))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch3))
  compiled-branch2
  (assign continue (label after-call1))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch3
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call1))

; 5.38
; ========================================================================
;; a.
(define (spread-arguments operands)
  (define (iter instruction-list rest-operands)
    (if (null? rest-operands)
	instruction-list
	(let ((first-operand (car rest-operands)))
	  (cond ((null? instruction-list)
		 (iter
		  (compile first-operand 'arg2 'next)
		  (cdr rest-operands)))
		((< (length instruction-list) 2)
		 (iter
		  (append-instruction-sequences
		   (compile first-operand 'arg1 'next)
		   instruction-list)
		  (cdr rest-operands)))
		(else
		 (iter
		  (preserving '(arg1 arg2)
			      (compile first-operand 'arg1 'next)
			      instruction-list)
		  (cdr rest-operands)))
	  ))))
  (iter '() operands))

;; (pp (compile '(+ 5 (* 2 3)) 'val 'return))
;; ((continue)
;;  (arg1 arg2 target)
;;  ((assign arg1 (const 3))
;; (assign arg2 (const 2))
;; (assign arg1 (op *) (reg arg1) (reg arg2))
;; (assign arg2 (const 5))
;; (assign val (op +) (reg arg1) (reg arg2))
;; (goto (reg continue))))

;; b.
;; Add this to book_code/ch5-compiler.scm
;;          ((cond? exp) (compile (cond->if exp) target linkage))
;; +       ((open-coded-primitive? exp)
;; +        (compile-open-coded-application exp target linkage))
;;          ((application? exp)

(define (open-coded-primitive? exp)
  (memq (operator exp) '(+ - = *)))

(define (compile-open-coded-application exp target linkage)
  (let ((primitive (operator exp)))
    (end-with-linkage
     linkage
     (preserving '(argl env)
		 (spread-arguments (operands exp))
		 (make-instruction-sequence '(arg1 arg2) '(target)
					    `((assign ,target (op ,primitive) (reg arg1) (reg arg2))))))))


(define (compile-equals exp target linkage)
  (make-primitive-application '= exp target linkage))

(define (compile-multiplication exp target linkage)
  (make-primitive-application '* exp target linkage))

(define (compile-subtraction exp target linkage)
  (make-primitive-application '- exp target linkage))

(define (compile-addition exp target linkage)
  (make-primitive-application '+ exp target linkage))

;; c.
(pp
 (compile
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n)))
 'val
 'next)
 )

;; ((env)
;;  (val)
;;  ((assign val (op make-compiled-procedure) (label entry154) (reg env))
;;   (goto (label after-lambda153))
;;   entry154
;;   (assign env (op compiled-procedure-env) (reg proc))
;;   (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;;   (assign arg1 (const 1))
;;   (assign arg2 (op lookup-variable-value) (const n) (reg env))
;;   (assign val (op =) (reg arg1) (reg arg2))
;;   (test (op false?) (reg val))
;;   (branch (label false-branch156))
;;   true-branch157
;;   (assign val (const 1))
;;   (goto (reg continue))
;;   false-branch156
;;   (save continue)
;;   (assign arg1 (op lookup-variable-value) (const n) (reg env))
;;   (assign proc (op lookup-variable-value) (const factorial) (reg env))
;;   (assign arg1 (const 1))
;;   (assign arg2 (op lookup-variable-value) (const n) (reg env))
;;   (assign val (op -) (reg arg1) (reg arg2))
;;   (assign argl (op list) (reg val))
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch160))
;;   compiled-branch159
;;   (assign continue (label proc-return161))
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;;   proc-return161
;;   (assign arg2 (reg val))
;;   (goto (label after-call158))
;;   primitive-branch160
;;   (assign arg2 (op apply-primitive-procedure) (reg proc) (reg argl))
;;   after-call158
;;   (assign val (op *) (reg arg1) (reg arg2))
;;   (restore continue)
;;   (goto (reg continue))
;;   after-if155
;;   after-lambda153
;;   (perform (op define-variable!) (const factorial) (reg val) (reg env))
;;   (assign val (const ok))))

;; This version is 42 lines instead of 81 - the open-coded primitives reduced
;; the function by almost half.  It also generates some unneccesary instructions
;; so it could be even more efficient if it were smarter.

;;d.
;; My version of (spread-arguments) already handles > 2 operands.

; 5.39
; ========================================================================
;; I've moved the lexical-address-lookup to lexical-addressing-compiler.scm

; 5.40
; ========================================================================


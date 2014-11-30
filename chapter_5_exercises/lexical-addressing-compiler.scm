(load "book_code/ch5-eceval-support.scm")
(load "book_code/ch5-compiler.scm")

;; find-variable

(define (compile-time-env-first-frame compile-time-env)
  (car compile-time-env))

(define (compile-time-env-rest-frames compile-time-env)
  (cdr compile-time-env))

(define (find-variable variable env)
  (define (var-index variables index)
    (if (eq? variable (car variables))
	index
	(var-index (cdr variables) (+ index 1)))
    )

  (define (iter frames frame-num)
    (if (null? frames)
	'not-found
	(let ((frame (compile-time-env-first-frame frames)))
	  (if (memq variable frame)
	      (list frame-num (var-index frame 0))
	      (iter (compile-time-env-rest-frames frames) (+ frame-num 1))
	      ))))

  (iter env 0)
  )

;; lexical-addressing
(define (make-address frame-number displacement)
  (cons frame-number displacement))

(define (address-frame-number address)
  (car address))

(define (address-displacement-number address)
  (cdr address))

(define (lexical-address-lookup address environment)
  (let ((frame (list-ref environment (address-frame-number address))))
    (let ((variable-value (list-ref (frame-values frame) (address-displacement-number address))))
      (if (eq? variable-value '*unassigned)
	  (error "Variable has no value")
	  variable-value)))
  )

(define (lexical-address-set! address environment value)
  (define (iter values current-index)
    (if (= current-index (address-displacement-number address))
	(set-car! values value)
	(iter (cdr values) (+ current-index 1))))

    (let ((frame (list-ref environment (address-frame-number address))))
      (iter (frame-values frame) 0)
      ))

;; compilation

(define (compile exp target linkage env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage env))
        ((quoted? exp) (compile-quoted exp target linkage env))
        ((variable? exp)
         (compile-variable exp target linkage env))
        ((assignment? exp)
         (compile-assignment exp target linkage env))
        ((definition? exp)
         (compile-definition exp target linkage env))
        ((if? exp) (compile-if exp target linkage env))
        ((lambda? exp) (compile-lambda exp target linkage env))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
			   env))
        ((cond? exp) (compile (cond->if exp) target linkage env))
        ((application? exp)
         (compile-application exp target linkage env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))


;;;SECTION 5.5.2

;;;linkage code

(define (compile-linkage linkage env)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence env)
  (preserving '(continue)
   instruction-sequence
   (compile-linkage linkage env)))

;;;simple expressions

(define (compile-self-evaluating exp target linkage env)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,exp))))
   env))

(define (compile-quoted exp target linkage env)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))
   env))

(define (compile-variable exp target linkage env)
  (end-with-linkage linkage
   (make-instruction-sequence '(env) (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg env))))
   env))

(define (compile-assignment exp target linkage env)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next env)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok)))))
     env)))

(define (compile-definition exp target linkage env)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next env)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok)))))
     env)))


(define (compile-if exp target linkage env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next env))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage env))
            (a-code
             (compile (if-alternative exp) target linkage env)))
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

;;; sequences

(define (compile-sequence seq target linkage env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage env)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next env)
       (compile-sequence (rest-exps seq) target linkage env))))

;;;lambda expressions

(define (compile-lambda exp target linkage env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env))))
	 env)
        (compile-lambda-body exp proc-entry env))
       after-lambda))))

(define (compile-lambda-body exp proc-entry compile-time-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return
		       (extend-compile-time-env formals compile-time-env)))))

;;;SECTION 5.5.3

;;;combinations

(define (compile-application exp target linkage env)
  (let ((proc-code (compile (operator exp) 'proc 'next env))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next env))
              (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage env)))))

(define (compile-procedure-call target linkage env)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage env))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
          (make-instruction-sequence '(proc argl)
                                     (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl))))
	  env)))
       after-call))))

;;;applying compiled procedures

(define (compile-proc-appl target linkage env)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                          (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry)
                        (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

'(LEXICAL ADDRESSING COMPILER LOADED)

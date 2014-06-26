(load "4_11_to_4_15.scm")

; 4.16
; ========================================================================
; a.
(define (lookup-variable-value var env)
  (define binding (find-variable-in-environment var env))

  (if (defined? binding)
      (if (eq? (binding-values binding) '*unassigned*)
	  (error "Unassigned variable -- LOOKUP-VARIABLE-VALUE" var)
	  (binding-values binding))))

; b.
(define (scan-out-defines exp)
  (define (is-define? clause)
    (and (pair? clause) (eq? (car clause) 'define)))

  (define (make-let-clauses variables values)
    (cons 'let
	  (cons
	   (map (lambda (variable value) (list variable ''*unassigned*)) variables values)
	   (map (lambda (variable value) (list 'set! variable value)) variables values))))

  (define (expression-body)
    (define (iter clause seen-defines?)
      (cond ((is-define? (car clause)) (iter (cdr clause) true))
	    ((and seen-defines?) (car clause))
	    (else (iter (cdr clause) false))))

    (iter exp false))

  (define (build-replacement-expression source-exp replacement-exp replacement-assignments)
    (if (null? source-exp)
	replacement-exp
	(if (is-define? (car source-exp))
	    (build-replacement-expression '() (append replacement-exp (list (append replacement-assignments (list (expression-body))))) '())
	    (build-replacement-expression (cdr source-exp) (append replacement-exp (list (car source-exp))) replacement-assignments))))

  (let ((defines (filter is-define? exp)))
    (define variables (map cadr defines))
    (define values (map caddr defines))

    (build-replacement-expression (cdr exp) (list (car exp)) (make-let-clauses variables values))
    ))

; c.
; I'm installing this in make-procedure because I think it is more obvious.
; It would be strange to call (procedure-body) on a clause and get back a transformed
; expression.
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

; 4.17
; ========================================================================
; Maybe later.

; 4.18
; ========================================================================
; If the defines are scanned out using our implementation from 4.16, the
; solve function will work as usual.

; If the defines are scanned out using the crazy new way where they get set!
; in a nested let block, it won't work. In the innermost block where a and
; b get set, they will still be referencing a frame in which y and dy
; have no definitions at all. Like so:
;; (define (solve f y0 dt)
;;   (let ((y '*unassigned*) (dy '*unassigned*))
;;     (let ((a (integral (delay dy) y0 dt))
;; 	     (b (stream-map f y)))
;;       (set! y a)
;;       (set! dy b))
;;     y))

; 4.19
; ========================================================================
; I argue for Ben's way.  It is the easiest to reason about as the developer.
; If I were to write this convoluted function, I'd expect it to work the way
; he describes.
;
; We could implement it Eva's way if we used the version of (scan-out-defines)
; from 4.18.  Then the frame that (+ a b) is evaluated in will have a set! to 5
; and b set! to 5 + 10 and Eva will get her precious 20.

; 4.20
; ========================================================================
; a.
(define (install-letrec-package)
  (define (letrec-expressions clause)
    (cadr clause))
  (define (letrec-body clause)
    (caddr clause))
  (define (letrec-variables clause)
    (map car (letrec-expressions clause)))
  (define (letrec-values clause)
    (map cadr (letrec-expressions clause)))

  (define (make-variable-unassignments exp)
    (define (unassign-variable variable)
      (list variable ''*unassigned*))

    (map unassign-variable (letrec-variables exp)))

  (define (make-variable-assignments exp)
    (define (assign-variable variable value)
      (list (symbol-append 'inner- variable)
	    value))

    (list 'let
	  (map assign-variable (letrec-variables exp) (letrec-values exp))))

  (define (make-sets exp)
    (define (set-variable inner-variable outer-variable)
      (list 'set! outer-variable inner-variable))

    (map set-variable
	 (map (lambda (var) (symbol-append 'inner- var)) (letrec-variables exp))
	 (letrec-variables exp)))

  (define (letrec->let exp)
    (list 'let
	  (make-variable-unassignments exp)
	  (append (make-variable-assignments exp) (make-sets exp))
	  (letrec-body exp)
	  )
    )

  (put 'transform 'letrec letrec->let)

  'letrec-package-installed)

(install-letrec-package)

; b.
; No seriously I'm not going to draw any environment diagrams right now.

; 4.21
; ========================================================================
; a. see tests
; Man that is really trippy.  Eventually I realized that if you call two
; consecutive lambdas, the second lambda is passed as an argument to the
; first.
; 1 ]=> ((lambda (x) (pp x)) (lambda (y) (pp "this is lambda 2")))
; (lambda (y)
;   (pp "this is lambda 2"))
; So (fact fact n) calls the fibonacci function and passes in the fibonacci
; function as an argument so it'll actually have a way to reference it.

; b. see tests

; 4.22
; ========================================================================
; First of all, I'd like to point out the definition of (eval) they give
; us to use in this chapter contradicts the version they had us implement
; in 4.3!  Which one is correct?? This is why I am reluctant to fully
; implement any of the exercises they give us for fear it'll have to be
; rewritten later.
(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
	((variable? exp) (analyze-variable exp))
	((get 'analyze (operator exp)) ((get 'analyze (operator exp)) exp))
	(else
	 (error "Unknown expression type -- ANALYZE" exp))))

(define (install-let-package)
  (define (let-expressions clause)
    (cadr clause))
  (define (let-body clause)
    (cddr clause))
  (define (let-variables clause)
    (map car (let-expressions clause)))
  (define (let-values clause)
    (map cadr (let-expressions clause)))

  (define (let->combination exp)
    (cons
     (make-lambda
      (let-variables exp)
      (let-body exp))
     (let-values exp)))

  (define (analyze-let exp)
    (lambda (env)
      (
       (analyze-lambda
	(make-lambda
	 (let-variables exp)
	 (let-body exp)))
       (let-variables exp)
       (let-body exp))
      ))

  (put 'transform 'let let->combination)
  (put 'analyze 'let analyze-let)

  (put 'eval 'let (lambda (exp env)
		    (eval
		     (let->combination exp)
		     env)))

  'let-package-installed)

(install-let-package)

; 4.23
; ========================================================================
; Eva's version analyzes every expression, then chains them together in a
; giant lambda.
; Alyssa's version analyzes every expression and leaves you a lambda that
; will call them all in a sequence.
; Her version doesn't do the work of chaining the lambdas together until
; execution time.
; For a statement with one argument they will do the same amount of work.
; For a statement with two arguments Eva's version does more work at
; analysis time.

; 4.24
; ========================================================================
(define (time-statement statement env num-executions)
  (define (eval-statement-n-times statement env)
    (define (repeat current)
      (if (< current num-executions)
	  (begin
	    (eval statement env)
	    (repeat (+ current 1)))))
    (repeat 0))

  (define start-time (real-time-clock))
  (pp (list "Start time: " start-time))
  (eval-statement-n-times statement env)
  (define end-time (real-time-clock))
  (pp (list "End time: " end-time))
  (pp (list "Time elapsed: " (- end-time start-time))))

(define factorial-exp '(define (factorial n)
			 (if (= n 1)
			     1
			     (* (factorial (- n 1)) n))))

(define (test-unanalyzed)
  (load "book_code/ch4-mceval.scm")
  (define the-global-environment (setup-environment))

  (time-statement factorial-exp the-global-environment 100000))

(define (test-analyzed)
  (load "book_code/ch4-analyzingmceval.scm")
  (define the-global-environment (setup-environment))

  (time-statement factorial-exp the-global-environment 100000))

; Since this isn't actually executing (factorial) we don't see any speed
; improvement from the analyzed version.

;; 1 ]=> (test-unanalyzed)
;; ;Loading "book_code/ch4-mceval.scm"... done
;; ("Start time: " 35074)
;; ("End time: " 36520)
;; ("Time elapsed: " 1446)
;; ;Unspecified return value

;; 1 ]=> (test-analyzed)
;; ;Loading "book_code/ch4-analyzingmceval.scm"...
;; ;  Loading "book_code/ch4-mceval.scm"... done
;; ;... done
;; ("Start time: " 17550)
;; ("End time: " 24420)
;; ("Time elapsed: " 6870)
;; ;Unspecified return value
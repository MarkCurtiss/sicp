(load "book_code/ch4-mceval.scm")

(define (delay-it exp env)
  (list 'thunk exp env))

(define (memoize-it exp env)
  (list 'memoize-thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (memoizing-force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (non-memoizing-force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (force-it exp)
  (if (memoized? exp)
      (memoizing-force-it exp)
      (non-memoizing-force-it exp)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (delayed? parameter)
  (and
   (pair? parameter)
   (eq? (cadr parameter) 'lazy)))

(define (memoized? parameter)
  (and
   (pair? parameter)
   (eq? (cadr parameter) 'lazy-memo)))

(define (list-of-possibly-delayed-args parameters exps env)
    (cond ((no-operands? exps)
	   '())
	  ((delayed? (car parameters))
	   (cons (delay-it (first-operand exps) env)
		 (list-of-possibly-delayed-args
		  (cdr parameters)
		  (rest-operands exps)
		  env)))
	  ((memoized? (car parameters))
	   (cons (memoize-it (first-operand exps) env)
		 (list-of-possibly-delayed-args
		  (cdr parameters)
		  (rest-operands exps)
		  env)))
	  (else (cons (actual-value (first-operand exps) env)
		      (list-of-possibly-delayed-args
		       (cdr parameters)
		       (rest-operands exps)
		       env)))))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-possibly-delayed-args (procedure-parameters procedure) arguments env)
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))
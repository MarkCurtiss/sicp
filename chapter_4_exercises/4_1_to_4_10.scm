; 4.1
; ========================================================================
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (eval exp env) exp)

(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env))
	    (right (list-of-values-right-to-left (rest-operands exps) env)))
      (cons right left))))

(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env))
	    (right (list-of-values-left-to-right (rest-operands exps) env)))
      (cons left right))))

; 4.2
; ========================================================================
;; a. Louis' modified (eval) will erroneously consider (define x 3) to be
;; procedure application.  (application?) is implemented by checking if the
;; exp is a (pair?).

;; b.
(define (operator exp) (car exp))
(define (louis-application? exp)
  (eq? (operator exp) 'call))

; 4.3
; ========================================================================
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (empty-list? exp)
  (eq? exp '()))
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
	((empty-list? exp) true)
        (else false)))

(define put 2d-put!)
(define get 2d-get)

(define (install-if-package)
  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
	(cadddr exp)
	'false))
  (define (true? x)
    (not (false? x)))

  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
	(eval (if-consequent exp) env)
	(eval (if-alternative exp) env)))

  (put 'eval 'if eval-if)

  'if-package-installed)

(install-if-package)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
	((get 'eval (operator exp)) ((get 'eval (operator exp)) exp env))
	(else
         (error "Unknown expression type -- EVAL" exp))))

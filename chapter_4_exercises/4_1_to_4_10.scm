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

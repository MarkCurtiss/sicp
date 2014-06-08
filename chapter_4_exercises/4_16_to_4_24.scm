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
;; 	  (b (stream-map f y)))
;;       (set! y a)
;;       (set! dy b))
;;     y))


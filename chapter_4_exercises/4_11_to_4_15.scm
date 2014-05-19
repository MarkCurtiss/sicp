(load "4_1_to_4_10.scm")

; 4.11
; ========================================================================
(define (make-frame variables values)
  (map (lambda (var val) (cons var val)) variables values))

(define (frame-variables frame)
  (map car frame))
(define (frame-values frame)
  (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (append! frame (list (cons var val))))

; 4.12
; ========================================================================
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (first-frame-binding frame)
  (car frame))
(define (rest-frame-bindings frame)
  (cdr frame))
(define (binding-variable binding)
  (car binding))
(define (binding-values binding)
  (cdr binding))
(define (set-binding-values! binding new-values)
  (set-cdr! binding new-values))

(define (find-variable-in-environment var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
	    ((eq? var (binding-variable (first-frame-binding frame)))
	     (first-frame-binding frame))
            (else (scan (rest-frame-bindings frame)))))
    (if (eq? env the-empty-environment)
	'()
        (let ((frame (first-frame env)))
          (scan frame))))

  (env-loop env))

(define (lookup-variable-value var env)
  (let ((binding (find-variable-in-environment var env)))
    (binding-values binding)))

(define (set-variable-value! var val env)
  (let ((binding (find-variable-in-environment var env)))
    (set-binding-values! binding val)))

(define (define-variable! var val env)
  (define binding (find-variable-in-environment var env))

  (if (null? binding)
      (add-binding-to-frame! var val (first-frame env))
      (set-binding-values! binding val)))

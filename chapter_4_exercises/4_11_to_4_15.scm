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
(define (defined? x) (not (null? x)))
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
(define (erase-binding! binding)
  (set-car! binding '()))

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
  (define binding (find-variable-in-environment var env))

  (if (defined? binding)
      (binding-values binding)))

(define (set-variable-value! var val env)
  (define binding (find-variable-in-environment var env))

  (if (defined? binding)
      (set-binding-values! binding val)))

(define (define-variable! var val env)
  (define binding (find-variable-in-environment var env))

  (if (defined? binding)
      (set-binding-values! binding val)
      (add-binding-to-frame! var val (first-frame env))))

; 4.13
; ========================================================================
;; I'm choosing to remove the bindings from all environments at once.
;; It is a rude implementation choice.  I am imagining a hapless
;; developer unbinding a variable in one environment only to find (hours
;; later) that it was actually removed from the entire universe at once!
(define (make-unbound! var env)
  (define binding (find-variable-in-environment var env))

  (if (defined? binding)
      (begin
	(erase-binding! binding)
	(make-unbound! var (enclosing-environment env)))))

; 4.14
; ========================================================================
;; (map) is needed for defining primitive procedures in the metacircular
;; evaluator's enviroment.

; 4.15
; ========================================================================
;; If (try try) runs forever, it means that (halts?) was not able to
;; determine that (try try) would run forever.
;; If (try try) halts, it means that (halts?) errantly terminated the execution
;; and broke the recursive nature of (try try).
;; Neither can be true, so there's no such thing as (halts?).
; 2.73
; ========================================================================
(define put 2d-put!)
(define get 2d-get)

(define (=number? exp num) (and (number? exp) (= exp num)))
(define variable? symbol?)
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
  (define variable? symbol?)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

;; a. Instead of switching on the operator using predicates like
;; (sum?) and (product?), we now look up the appropriate implementation
;; of (deriv) for our operator.  Essentially we've given each operator
;; its own type.  (number?) and (variable?) shouldn't be part of this
;; data-driven dispatch since they don't depend on the 'type' of the
;; expression at all.

;; b.
(define (install-sum-package)
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

  (put 'deriv '+
       (lambda (exp var)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var))))
  'done)

(define (install-multiplication-package)
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

  (put 'deriv '*
       (lambda (exp var)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp)))))
  'done)


;; 1 ]=> (install-sum-package)
;; Value: done
;; 1 ]=> (install-multiplication-package)
;; Value: done

;; 1 ]=> (deriv '(+ x 3) 'x)
;; ;Value: 1
;; 1 ]=> (deriv '(* x y) 'x)
;; ;Value: y

;; c.
(define (install-exponentiation-package)
  (define (base x) (car x))
  (define (power x) (cadr x))
  (define (make-expt base power)
    (cond ((=number? power 0) 1)
	  ((=number? power 1) base)
	  ((and (number? base) (number? power)) (expt base power))
	  (else (list '** base power))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

  (put 'deriv '**
       (lambda (exp var)
	 (make-product
	  (base exp)
	  (make-expt (base exp)
		     (- (power exp) 1)))))
  'done)


;; 1 ]=> (install-exponentiation-package)
;; Value: done

;; 1 ]=> (deriv '(** a 8) 'a)
;; Value 16: (* a (** a 7))

;; d.
(define (special-deriv exp var)
  (define variable? symbol?)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get (operator exp) 'special-deriv) (operands exp) var))))

;; We have to change the packages to put their entries into
;; the dispatch table differently.  Everything else will
;; remain the same.

(define (install-special-sum-package)
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

  (put '+ 'special-deriv
       (lambda (exp var)
	 (make-sum (special-deriv (addend exp) var)
		   (special-deriv (augend exp) var))))
  'done)

(define (install-special-multiplication-package)
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

  (put '* 'special-deriv
       (lambda (exp var)
         (make-sum
           (make-product (multiplier exp)
                         (special-deriv (multiplicand exp) var))
           (make-product (special-deriv (multiplier exp) var)
                         (multiplicand exp)))))
  'done)

(define (install-special-exponentiation-package)
  (define (base x) (car x))
  (define (power x) (cadr x))
  (define (make-expt base power)
    (cond ((=number? power 0) 1)
	  ((=number? power 1) base)
	  ((and (number? base) (number? power)) (expt base power))
	  (else (list '** base power))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

  (put '** 'special-deriv
       (lambda (exp var)
	 (make-product
	  (base exp)
	  (make-expt (base exp)
		     (- (power exp) 1)))))
  'done)


;; 1 ]=> (install-special-sum-package)
;; Value: done
;; 1 ]=> (special-deriv '(+ x 3) 'x)
;; Value: 1

;; 1 ]=> (install-special-multiplication-package)
;; Value: done
;; 1 ]=> (special-deriv '(* x y) 'x)
;; Value: y


;; 1 ]=> (install-special-exponentiation-package)
;; Value: done
;; 1 ]=> (special-deriv '(** a 8) 'a)
;; Value 18: (* a (** a 7))

; 2.74
; ========================================================================
;; a.
(define (get-record employee-id division-file)
  ((get 'get-record (type-tag division-file))))

;; An individual division can be in any structure it wants, provided it
;; has type information identifying employee records as well as type
;; information identifying the division.  Also, each division will have to
;; supply its own (get-record) implementation.

;; b.
(define (get-salary employee-record)
  ((get 'get-salary (division employee-record))))
;; This'll only work if each employee record contains a tag stating which
;; division it's in - that way we can look up the appropriate (get-salary)
;; implementation in the dispatch table.

;; c.
(define (find-employee-record name division-files)
  (if (null? division-files)
      '()
      (let ((current-file (car division-files)))
	(if (null? current-file)
	    '()
	    (let ((employee-record (get-record name current-file)))
	      (if (null? employee-record)
		  (find-employee-record (cdr division-files))
		  employee-record))))))

;; d.
;; Any new company will require a new implentation of (get-record) and
;; (get-salary) as well as division information on their personnel files.

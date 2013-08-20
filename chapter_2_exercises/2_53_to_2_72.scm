; 2.53
; ========================================================================
;; My guess: (a b c)
;; 1 ]=> (list 'a 'b 'c)
;; Value 12: (a b c)

;; My guess: ((george))
;; 1 ]=> (list (list 'george))
;; Value 13: ((george))

;; My guess: (y1 y2)
;; 1 ]=> (cdr '((x1 x2) (y1 y2)))
;; Value 14: ((y1 y2))

;; My guess: y1
;; 1 ]=> (cadr '((x1 x2) (y1 y2)))
;; Value 15: (y1 y2)
;; Dang man why does cdr give you a nested list?
;; Oh I guess because cdr always returns a list and the contents are also
;; a list.

;; My guess: #f
;; 1 ]=> (pair? (car '(a short list)))
;; Value: #f

;; My guess: #f
;; 1 ]=> (memq 'red '((red shoes) (blue socks)))
;; Value: #f

;; My guess: (red shoes blue socks)
;; 1 ]=> (memq 'red '(red shoes blue socks))
;; Value 19: (red shoes blue socks)

; 2.54
; ========================================================================
;; (equal? '(this is a list) '(this is a list))
;; #t

;; (equal? '(this is a list) '(this (is a) list))
;; #f

(define (my-equal? a b)
  (cond ((and (not (pair? a))
	      (not (pair? b))) (eq? a b))
	((and (pair? a)
	      (pair? b)) (and (eq? (car a) (car b))
			      (my-equal? (cdr a) (cdr b))))
	(else #f)))


;; 1 ]=> (my-equal? '(this is a list) '(this is a list))
;; Value: #t
;; 1 ]=> (my-equal? '(this is a list) '(this (is a) list))
;; Value: #f

; 2.55
; ========================================================================
;; What they typed is logically equivalent to:

;; (quote (quote abracadabra))

;; Uh and the (car) of that is clearly gonna be the symbol 'quote'


; 2.56
; ========================================================================
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (sum? x)  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (base exp)
	  (make-expt (base exp)
		     (- (power exp) 1))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**))
)
(define (base x) (cadr x))
(define (power x) (caddr x))
(define (make-expt base power)
    (cond ((=number? power 0) 1)
	  ((=number? power 1) base)
	  ((and (number? base) (number? power)) (expt base power))
	  (else (list '** base power)))
)

;; 1 ]=> (make-expt 2 0)
;; Value: 1
;; 1 ]=> (make-expt 8 1)
;; Value: 8
;; 1 ]=> (make-expt 2 5)
;; Value: 32

;; 1 ]=> (deriv (make-expt 'a 8) 'a)
;; Value 37: (* a (** a 7))

; 2.57
; ========================================================================
(define (addend s) (cadr s))
(define (augend s)
  (cond ((> (length s) 3) (cons '+ (cddr s)))
	(else (caddr s))))

(define (multiplier p) (cadr p))
(define (multiplicand p)
  (cond ((> (length p) 3) (cons '* (cddr p)))
	(else (caddr p))))

;; 1 ]=> (deriv '(* x y (+ x 3)) 'x)
;; Value 165: (+ (* x y) (* y (+ x 3)))

; 5.23
; ========================================================================
;; I added the following to eceval
;;   (test (op cond?) (reg exp))
;;   (branch (label ev-cond))
;; ....
;; ev-cond
;;   (assign exp (op cond->if) (reg exp))
;;   (goto (label eval-dispatch))

(define x 9)
(cond ((> x 0) x)
      ((= x 0) (display zero) 0)
      (else (- x)))

;;; EC-Eval input:
(define x 9)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(cond ((> x 0) x)
      ((= x 0) (display zero) 0)
      (else (- x)))

(total-pushes = 11 maximum-depth = 8)
;;; EC-Eval value:
9


; 5.24
; ========================================================================
; See my changes to ch5-eceval.scm for my implementation of (cond).
(load "book_code/load-eceval.scm")
(define the-global-environment (setup-environment))
(start eceval)

(cond)
;;; EC-Eval value:
;; ()
(define x 9)
(cond (else (- x 1)))
;;; EC-Eval value:
;; 8

(define x 9)
(cond ((> x 10) x)
      ((= x 0) (+ x 2) (* x 2))
      ((= x 9) (+ x 1) (* x 10))
      (else (- x 1)))
;;; EC-Eval value:
;; 90

(define x 8)
(cond ((> x 10) x)
      ((= x 0) (+ x 2) (* x 2))
      ((= x 9) (+ x 1) (* x 10))
      (else (- x 1)))

;;; EC-Eval value:
;; 7

; 5.25
; ========================================================================
;; No!

; 5.26
; ========================================================================
(load "book_code/load-eceval.scm")
(define the-global-environment (setup-environment))
(start eceval)

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;;  EC-Eval input:
;; (factorial 3)

;; (total-pushes = 134 maximum-depth = 10)
;;  EC-Eval value:
;; 6

;;  EC-Eval input:
;; (factorial 4)

;; (total-pushes = 169 maximum-depth = 10)
;;  EC-Eval value:
;; 24

;;  EC-Eval input:
;; (factorial 5)

;; (total-pushes = 204 maximum-depth = 10)
;;  EC-Eval value:
;; 120

;;  EC-Eval input:
;; (factorial 6)

;; (total-pushes = 239 maximum-depth = 10)
;;  EC-Eval value:
;; 720

; a.  The maximum depth is 10.
; b. Total pushes = 64 + 35*(n - 1)
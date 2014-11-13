; 5.31
; ========================================================================
;; 1. saves and restores the env register around the evaluation of the operator
;; 2. saves and restores env around the evaluation of each operand (except the final one)
;; 3. saves and restores argl around the evaluation of each operand
;; 4. saves and restores proc around the evaluation of the operand sequence

(f 'x 'y)
;; You could optimize out 1, 2, 3, 4

((f) 'x 'y)
;; You could optimize out 1, 2, 3, 4

(f (g 'x) y)
;; You could optimize out 1, 2

(f (g 'x) 'y)
;; You could optimize out 1, 2

; 5.32
; ========================================================================
;; a.

;; Put the following in ch5-eceval.scm

;; +(define (application-of-symbol? exp)
;; +  (and (application? exp)
;; +       (symbol? (operator exp))))

;;     (list 'application? application?)
;; +   (list 'application-of-symbol? application-of-symbol?)

;;    (branch (label ev-begin))
;; +  (test (op application-of-symbol?) (reg exp))
;; +  (branch (label ev-symbol-application))

;; +ev-symbol-application
;; +  (save continue)
;; +  (assign unev (op operands) (reg exp))
;; +  (save unev)
;; +  (assign exp (op operator) (reg exp))
;; +  (assign continue (label ev-appl-did-operator))
;; +  (goto (label eval-dispatch))

;; Notice that ev-symbol-application doesn't save (env) before jumping
;; to eval-dispatch.

;; b.
;; I am against this idea!  It would make the evaluator much more complex.
;; Also you'd have scads of code in the evaluator that was related solely
;; to performance optimizations rather than the syntax and semantics of
;; your language.

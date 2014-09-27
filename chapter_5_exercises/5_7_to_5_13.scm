; 5.7
; ========================================================================
;; See tests.

; 5.8
; ========================================================================
;; (load "book_code/ch5-regsim.scm")
;; (define ambiguous-machine
;;   (make-machine
;;    '(a)
;;    (list '())
;;    '(
;;      start
;;      (goto (label here))
;;      here
;;      (assign a (const 3))
;;      (goto (label there))
;;      here
;;      (assign a (const 4))
;;      (goto (label there))
;;      there
;;      ))
;;   )

;; (start ambiguous-machine)
;; (get-register-contents ambiguous-machine 'a)

;; 1 ]=>
;; Value: 3

;; For my fixes to detect redundant labels, see book_code/ch5-regsim.scm
;; inside of (extract-labels):
;; (if (memq next-inst (map car labels))
;;   (error "Redundant label detected:" next-inst)

; 5.9
; ========================================================================
;; See my changes to book_code/ch5-regsim.scm
;; In (make-operation-exp):
;; (if (list-search-positive (operation-exp-operands exp) label-exp?)
;;     (error "Attempted to apply an operation to a label -- MAKE-OPERATION-EXP")

; 5.10
; ========================================================================
;; Nice try, SICP!  But once bitten, twice shy.  If I change the syntax
;; for this evaluator we've built up I just now it is going to cause
;; problems down the road: you'll ask me to do some other change that is
;; incompatible with the new syntax.  Or you'll give me code to run that uses
;; the existing syntax.  I'm going to make my changes to a separate interpreter
;; defined in alternate-syntax-regsim.scm.  See the accompanying tests.
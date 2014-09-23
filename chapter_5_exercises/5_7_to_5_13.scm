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

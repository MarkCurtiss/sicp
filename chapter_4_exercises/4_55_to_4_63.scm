; 4.55
; ========================================================================
;; To run this code:
(load "book_code/ch4-query.scm")
(initialize-data-base microshaft-data-base)
(query-driver-loop)

;; a. all people supervised by Ben Bitdiddle
;;; Query input:
(supervisor ?x (Bitdiddle Ben))

;;; Query results:
(supervisor (tweakit lem e) (bitdiddle ben))
(supervisor (fect cy d) (bitdiddle ben))
(supervisor (hacker alyssa p) (bitdiddle ben))

;; b. the names and jobs of all people in the accounting division
;;; Query input:
(job ?x (accounting . ?y))

;;; Query results:
(job (cratchet robert) (accounting scrivener))
(job (scrooge eben) (accounting chief accountant))

;; c. the names and addresses of all people who live in Slumerville
;;; Query input:
;; (address ?x (Slumerville . ?y))

;;; Query results:
;; (address (aull dewitt) (slumerville (onion square) 5))
;; (address (reasoner louis) (slumerville (pine tree road) 80))
;; (address (bitdiddle ben) (slumerville (ridge road) 10))

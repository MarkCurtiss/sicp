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
(address ?x (Slumerville . ?y))

;;; Query results:
(address (aull dewitt) (slumerville (onion square) 5))
(address (reasoner louis) (slumerville (pine tree road) 80))
(address (bitdiddle ben) (slumerville (ridge road) 10))


; 4.56
; ========================================================================
;; a. the names of all people who are supervised by Ben Bitdiddle, together
;; with their addresses

;;; Query input:
(and (supervisor ?person (bitdiddle ben))
     (address ?person ?where))

;;; Query results:
(and (supervisor (tweakit lem e) (bitdiddle ben)) (address (tweakit lem e) (boston (bay state road) 22)))
(and (supervisor (fect cy d) (bitdiddle ben)) (address (fect cy d) (cambridge (ames street) 3)))
(and (supervisor (hacker alyssa p) (bitdiddle ben)) (address (hacker alyssa p) (cambridge (mass ave) 78)))

;; b. all people whose salary is less than Ben Bitdiddle's, together with
;; their salary and Ben Bitdiddle's salary
;;; Query input:
(and (salary ?person ?amount)
     (salary (bitdiddle ben) ?bensalary)
     (lisp-value < ?amount ?bensalary))

;;; Query results:
(and (salary (aull dewitt) 25000) (salary (bitdiddle ben) 60000) (lisp-value < 25000 60000))
(and (salary (cratchet robert) 18000) (salary (bitdiddle ben) 60000) (lisp-value < 18000 60000))
(and (salary (reasoner louis) 30000) (salary (bitdiddle ben) 60000) (lisp-value < 30000 60000))
(and (salary (tweakit lem e) 25000) (salary (bitdiddle ben) 60000) (lisp-value < 25000 60000))
(and (salary (fect cy d) 35000) (salary (bitdiddle ben) 60000) (lisp-value < 35000 60000))
(and (salary (hacker alyssa p) 40000) (salary (bitdiddle ben) 60000) (lisp-value < 40000 60000))

;; c. all people who are supervised by someone who is not in the computer
;; division, together with the supervisor's name and job.
;;; Query input:
(and
 (supervisor ?person ?boss)
 (not (job ?boss (computer . ?title)))
 (job ?boss (?division . ?title)))

;;; Query results:
(and (supervisor (aull dewitt) (warbucks oliver)) (not (job (warbucks oliver) (computer big wheel))) (job (warbucks oliver) (administration big wheel)))
(and (supervisor (cratchet robert) (scrooge eben)) (not (job (scrooge eben) (computer chief accountant))) (job (scrooge eben) (accounting chief accountant)))
(and (supervisor (scrooge eben) (warbucks oliver)) (not (job (warbucks oliver) (computer big wheel))) (job (warbucks oliver) (administration big wheel)))
(and (supervisor (bitdiddle ben) (warbucks oliver)) (not (job (warbucks oliver) (computer big wheel))) (job (warbucks oliver) (administration big wheel)))

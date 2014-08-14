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

; 4.57
; ========================================================================
;; Note that I couldn't get this rule to work in the evaluator until I
;; looked online and found other people using (assert!) in their solutions.
;; Why do they give us exercises that don't work with the implementation
;; they've given us so far??
(assert!
 (rule (can-replace ?replacement ?replacee)
       (and
	(job ?replacement ?replacementjob)
	(job ?replacee ?replaceejob)
	(not (same ?replacement ?replacee))
	(or
	 (same ?replacementjob ?replaceejob)
	 (can-do-job ?replacementjob ?replaceejob))))
 )

;; a. all people who can replace Cy D. Fect
;;; Query input:
(can-replace ?x (Fect Cy D))

;;; Query results:
(can-replace (hacker alyssa p) (fect cy d))
(can-replace (bitdiddle ben) (fect cy d))

;; b. all people who can replace someone who is being paid more than they
;; are, together with the two salaries
;;; Query input:
(and
 (salary ?replacement ?replacementsalary)
 (salary ?replacee ?replaceesalary)
 (lisp-value > ?replaceesalary ?replacementsalary)
 (can-replace ?replacement ?replacee))

;;; Query results:
(and (salary (aull dewitt) 25000) (salary (warbucks oliver) 150000) (lisp-value > 150000 25000) (can-replace (aull dewitt) (warbucks oliver)))
(and (salary (fect cy d) 35000) (salary (hacker alyssa p) 40000) (lisp-value > 40000 35000) (can-replace (fect cy d) (hacker alyssa p)))

; 4.58
; ========================================================================
(assert!
 (rule (big-shot ?person ?division)
       (and (job ?person (?division . ?x))
	    (or
	     (not (supervisor ?person ?supervisor))
	     (and
	      (supervisor ?person ?supervisor)
	      (not (job ?supervisor (?division . ?y)))))))
 )

;;; Query input:
(big-shot ?x ?y)

;;; Query results:
(big-shot (warbucks oliver) administration)
(big-shot (scrooge eben) accounting)
(big-shot (bitdiddle ben) computer)

; 4.59
; ========================================================================
;; How am I supposed to add data to the database?  They have not shown us!
;; Maybe (assert!) will work here as well?  Is this whole chapter a
;; meta-lesson in always reading the entire source before implementing
;; anything?
(assert! (meeting acounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))

;; a. On Friday morning, Ben wants to query the data base for all the
;; meetings that occur that day. What query should he use?
;;; Query input:
(meeting ?org (Friday ?time))

;;; Query results:
(meeting administration (friday 1pm))

;; b. Alyssa P. Hacker is unimpressed. She thinks it would be much more
;; useful to be able to ask for her meetings by specifying her name. So
;; she designs a rule that says that a person's meetings include all
;; whole-company meetings plus all meetings of that person's division.
;; Fill in the body of Alyssa's rule.
(assert!
 (rule (meeting-time ?person ?day-and-time)
       (and
	(job ?person (?division . ?y))
	(or
	 (meeting whole-company ?day-and-time)
	 (meeting ?division ?day-and-time))))
 )

;;; Query input:
(meeting-time (hacker alyssa p) ?time)

;;; Query results:
(meeting-time (hacker alyssa p) (wednesday 4pm))
(meeting-time (hacker alyssa p) (wednesday 3pm))

;; c. Alyssa arrives at work on Wednesday morning and wonders what meetings
;; she has to attend that day. Having defined the above rule, what query
;; should she make to find this out?
;;; Query input:
(meeting-time (hacker alyssa p) (Wednesday ?time))

;;; Query results:
(meeting-time (hacker alyssa p) (wednesday 4pm))
(meeting-time (hacker alyssa p) (wednesday 3pm))

; 4.60
; ========================================================================
;; This happens because either combination of the two people satisfies
;; the rule.
;; I don't think there is a way to get the unique pairs with the language
;; given to us, as we don't have any way to filter result sets.

; 4.61
; ========================================================================
(assert!
 (rule (?x next-to ?y in (?x ?y . ?u)))
 )

(assert!
 (rule (?x next-to ?y in (?v . ?z))
       (?x next-to ?y in ?z))
 )

;;; Query input:
(?x next-to ?y in (1 (2 3) 4))

;;; Query results:
((2 3) next-to 4 in (1 (2 3) 4))
(1 next-to (2 3) in (1 (2 3) 4))

;;; Query input:
(?x next-to 1 in (2 1 3 1))

;;; Query results:
(3 next-to 1 in (2 1 3 1))
(2 next-to 1 in (2 1 3 1))

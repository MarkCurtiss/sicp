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

; 4.62
; ========================================================================
(assert!
 (rule (last-pair (?x) (?x)))
 )

(assert!
 (rule (last-pair (?a . ?b) (?c))
       (last-pair ?b (?c)))
 )


;;; Query input:
(last-pair (3) ?x)

;;; Query results:
(last-pair (3) (3))

;;; Query input:
(last-pair (1 2 3) ?x)

;;; Query results:
(last-pair (1 2 3) (3))

;;; Query input:
(last-pair (2 ?x) (3))

;;; Query results:
(last-pair (2 3) (3))

;; Do your rules work correctly on queries such as (last-pair ?x (3))
;; No.
;;; Query input:
(last-pair ?x (3))

;;; Query results:
;Aborting!: maximum recursion depth exceeded

; 4.63
; ========================================================================
(load "book_code/ch4-query.scm")

(define genesis-data-base
  '(
    (son Adam Cain)
    (son Cain Enoch)
    (son Enoch Irad)
    (son Irad Mehujael)
    (son Mehujael Methushael)
    (son Methushael Lamech)
    (wife Lamech Ada)
    (son Ada Jabal)
    (son Ada Jubal)

    (rule (son-of ?father ?son)
	  (or
	   (son ?father ?son)
	   (and
	    (wife ?father ?wife)
	    (son ?wife ?son))
	   ))

    (rule (grandson-of ?grandfather ?grandson)
	  (and
	   (son-of ?grandfather ?father)
	   (son-of ?father ?grandson)))
    ))

(initialize-data-base genesis-data-base)
(query-driver-loop)

;;find the grandson of Cain
;;; Query input:
(grandson-of cain ?x)

;;; Query results:
(grandson-of cain irad)

;;the sons of Lamech
;;; Query input:
(son-of lamech ?x)

;;; Query results:
(son-of lamech jubal)
(son-of lamech jabal)

;;the grandsons of Methushael
;;; Query input:
(grandson-of methushael ?x)

;;; Query results:
(grandson-of methushael jubal)
(grandson-of methushael jabal)

; 4.64
; ========================================================================
;; The application of (outranked-by) in the (and) clause will recurse
;; indefinitely, as the (supervisor) clause will never fail to produce
;; a valid frame.

; 4.65
; ========================================================================
;; Our query language has no mechanism for producing unique results and
;; Oliver Warbucks is the manager of 2 middle managers, one of whom (Ben
;; Bitdiddle) has three people reporting to him.

; 4.66
; ========================================================================
;; Ben's mapping function will have the same issue with duplicate values as
;; Cy D. Fect discovered in 4.65.
;; One way to deal with this would be to add a (unique) operator to the
;; query language - this would remove duplicate values from the unified frame.

; 4.67
; ========================================================================
;; We maintain a hash keyed on frame+pattern+datum and before we add a new
;; frame to the stream we first check if that combination already exists in
;; the hash.  If so, we don't add the new frame.

; 4.68
; ========================================================================
(assert!
 (rule (append-to-form () ?y ?y))
 )

(assert!
 (rule (append-to-form (?u . ?v) ?y (?u . ?z))
       (append-to-form ?v ?y ?z))
 )

(assert!
 (rule (reverse (?y) (?y)))
 )

(assert!
 (rule (reverse (?car . ?cdr) ?reversed-list)
       (and
	(reverse ?cdr ?reversed-cdr)
	(append-to-form ?reversed-cdr (?car) ?reversed-list)))
 )

;;; Query input:
(reverse (1 2 3) ?x)

;;; Query results:
(reverse (1 2 3) (3 2 1))

;;; Query input:
(reverse ?x (1 2 3))

;;; Query results:
;; This recurses indefinitely.

; 4.69
; ========================================================================
(assert!
 (rule (ends-in-grandson (grandson)))
 )

(assert!
 (rule (ends-in-grandson (?x . ?y))
       (ends-in-grandson ?y))
 )

(assert!
 (rule ((great grandson) ?grandfather ?great-grandson)
       (and (son-of ?grandfather ?son)
	    (grandson-of ?son ?great-grandson)))
 )

(assert!
 (rule ((great . ?relationship) ?x ?y)
       (and
	(son-of ?x ?father)
	(?relationship ?father ?y)
	(ends-in-grandson ?relationship)))
 )

;;; Query input:
(?relationship adam irad)

;;; Query results:
((great . grandson) adam irad)

;;; Query input:
((great grandson) ?g ?ggs)

;;; Query results:
((great grandson) mehujael jubal)
((great grandson) irad lamech)
((great grandson) mehujael jabal)
((great grandson) enoch methushael)
((great grandson) cain mehujael)
((great grandson) adam irad)

;;; Query input:
(?relationship adam jabal)

;;; Query results:
((great great great great great grandson) adam jabal)

; 4.70
; ========================================================================
;; Without the (let) binding the old rules to a variable, the
;; (cons-stream) call would create a stream consisting solely of the latest
;; rule.

; 4.71
; ========================================================================
;; Both implementations will recurse indefinitely but the book's will let
;; you see the output from each iteration since (apply-rule) gets 
;; evaluated as part of (display-stream).

;; Here is the output from using the book's implementation:
;;; Query input:
(assert! (rule (recurses ?x) (or (same ?x ?x) (recurses ?x))))

Assertion added to data base.

;;; Query input:
(recurses (bitdiddle ben))

;;; Query results:
(recurses (bitdiddle ben))
(recurses (bitdiddle ben))
(recurses (bitdiddle ben))
;;; continues forever

;; Here is the output from Louis Reasoner's version:
;;; Query input:
(assert! (rule (recurses ?x) (or (same ?x ?x) (recurses ?x))))

Assertion added to data base.
;;; Query input:
(recurses (bitdiddle ben))

;;; Query results:
;Aborting!: maximum recursion depth exceeded

; 4.72
; ========================================================================
;; If the first stream or both streams are infinite and you don't interleave
;; them, you'll only ever see elements from the first infinite stream you
;; encounter.  See 4.72.

; 4.73
; ========================================================================
;; Again, it protects you from infinite recursion in the case where the
;; stream is self-referential.

; 4.74
; ========================================================================
;; a.
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (s) (not (stream-null? s))) stream)))

;; b.
;; No.

; 4.75
; ========================================================================
;; See my additions to book_code/ch4-query.scm.

;;; Query input:
(unique (job ?x (computer wizard)))

;;; Query results:
(unique (job (bitdiddle ben) (computer wizard)))

;; Test your implementation by forming a query that lists all people who supervise precisely one person.
;;; Query input:
(and (supervisor ?person ?boss) (unique (supervisor ?x ?boss)))

;;; Query results:
(and (supervisor (cratchet robert) (scrooge eben)) (unique (supervisor (cratchet robert) (scrooge eben))))
(and (supervisor (reasoner louis) (hacker alyssa p)) (unique (supervisor (reasoner louis) (hacker alyssa p))))

; 4.76
; ========================================================================
;; See my changes to conjoin in book_code/ch4-query.scm.

;;; Query input:
(and (salary ?person ?amount) (job ?person ?division))

;;; Query results:
(and (salary (bitdiddle ben) 60000) (job (bitdiddle ben) (computer wizard)))
(and (salary (hacker alyssa p) 40000) (job (hacker alyssa p) (computer programmer)))
(and (salary (fect cy d) 35000) (job (fect cy d) (computer programmer)))
(and (salary (tweakit lem e) 25000) (job (tweakit lem e) (computer technician)))
(and (salary (reasoner louis) 30000) (job (reasoner louis) (computer programmer trainee)))
(and (salary (warbucks oliver) 150000) (job (warbucks oliver) (administration big wheel)))
(and (salary (scrooge eben) 75000) (job (scrooge eben) (accounting chief accountant)))
(and (salary (cratchet robert) 18000) (job (cratchet robert) (accounting scrivener)))
(and (salary (aull dewitt) 25000) (job (aull dewitt) (administration secretary)))
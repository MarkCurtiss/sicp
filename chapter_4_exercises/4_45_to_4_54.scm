; 4.45
; ========================================================================
(define (require p) (if (not p) (amb)))
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define *unparsed* '())

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))
;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:
(parse '(the professor lectures to the student in the class with the cat))

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:

;;; Starting a new problem
;;; Amb-Eval value:
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb-phrase (verb lectures)
		(prep-phrase (prep to)
			     (simple-noun-phrase (article the) (noun student))))
   (prep-phrase (prep in)
		(simple-noun-phrase (article the) (noun class))))
  (prep-phrase (prep with)
	       (simple-noun-phrase (article the) (noun cat)))))

;; This version means the professor is lecturing to the student and the professor has a cat.

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase (verb lectures) (prep-phrase (prep to)
					    (simple-noun-phrase (article the) (noun student))))
  (prep-phrase (prep in)
	       (noun-phrase
		(simple-noun-phrase (article the) (noun class))
		(prep-phrase (prep with)
			     (simple-noun-phrase (article the) (noun cat)))))))

;; The professor is giving a lecture to the student and the student is in the class that has the cat.

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase (verb-phrase (verb lectures)
			   (prep-phrase (prep to)
					(noun-phrase
					 (simple-noun-phrase (article the) (noun student))
					 (prep-phrase (prep in)
						      (simple-noun-phrase (article the) (noun class))))))
	      (prep-phrase (prep with)
			   (simple-noun-phrase (article the) (noun cat)))))

;; The professor lectures to the student in the class, and the professor is lecturing with the cat.

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase (verb lectures)
	      (prep-phrase (prep to)
			   (noun-phrase
			    (noun-phrase (simple-noun-phrase (article the) (noun student))
					 (prep-phrase (prep in)
						      (simple-noun-phrase (article the) (noun class))))
			    (prep-phrase (prep with)
					 (simple-noun-phrase (article the) (noun cat)))))))

;; the professor lectures to the student who is in the class and the student has a cat.

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase (verb lectures)
	      (prep-phrase (prep to)
			   (noun-phrase (simple-noun-phrase (article the) (noun student))
					(prep-phrase (prep in)
						     (noun-phrase
						      (simple-noun-phrase (article the) (noun class))
						      (prep-phrase (prep with)
								   (simple-noun-phrase (article the) (noun cat)))))))))

;; The professor lectures to the student in the classroom and the classroom has a cat.

;;; Amb-Eval input:
try-again

;;; There are no more values of
(parse (quote (the professor lectures to the student in the class with the cat)))


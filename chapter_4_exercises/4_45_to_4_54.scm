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

; 4.46
; ========================================================================
;; All of our parsing methods are written assuming left-to-right!
;; If we parsed right-to-left our noun phrases would go noun -> article, or
;; 'cat the'.

; 4.47
; ========================================================================
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

;; This works so long as (parse-word) succeeds - as soon as it doesn't,
;; we get an infinite loop.  If we change the order of the arguments,
;; we immediately get an infinite loop.  It is because we are calling
;; parse-verb-phrase without a base case to terminate recursion!

;;; Amb-Eval input:
(define (parse-verb-phrase)
  (amb (list 'verb-phrase
	     (parse-verb-phrase)
	     (parse-prepositional-phrase))
       (parse-word verbs)))


;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:
(parse '(the professor lectures to the student in the class with the cat))

;;; Starting a new problem
;; (this goes on forever and doesn't terminate).

; 4.48
; ========================================================================
(define adjectives '(adjective sleepy cranky stoned))

(define (parse-adjective-phrase)
  (list 'adjective-phrase
	(parse-word articles)
	(parse-word adjectives)
	(parse-word nouns)))

(define (parse-simple-noun-phrase)
  (amb
   (list 'simple-noun-phrase
	 (parse-word articles)
	 (parse-word adjectives)
	 (parse-word nouns))
   (list 'simple-noun-phrase
	 (parse-word articles)
	 (parse-word nouns))))

(parse '(the cranky professor lectures to the stoned student in the class with the sleepy cat))

;;; Starting a new problem
;;; Amb-Eval value:
;; (sentence
;;  (simple-noun-phrase (article the) (adjective cranky) (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb-phrase (verb lectures)
;; 		(prep-phrase (prep to)
;; 			     (simple-noun-phrase (article the) (adjective stoned) (noun student))))
;;    (prep-phrase (prep in)
;; 		(simple-noun-phrase (article the) (noun class))))
;;   (prep-phrase (prep with)
;; 	       (simple-noun-phrase (article the) (adjective sleepy) (noun cat)))))

; 4.49
; ========================================================================
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (set! *unparsed* (cdr *unparsed*))
  (list (car word-list) (an-element-of (cdr word-list))))


;;; Amb-Eval input:
(parse '(the professor lectures to the student))

;;; Starting a new problem
;;; Amb-Eval value:
(sentence (simple-noun-phrase (article the) (noun student)) (verb-phrase (verb studies) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(sentence (simple-noun-phrase (article the) (noun student)) (verb-phrase (verb studies) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun professor)))))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(sentence (simple-noun-phrase (article the) (noun student)) (verb-phrase (verb studies) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun cat)))))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(sentence (simple-noun-phrase (article the) (noun student)) (verb-phrase (verb studies) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun class)))))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(sentence (simple-noun-phrase (article the) (noun student)) (verb-phrase (verb studies) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun student)))))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(sentence (simple-noun-phrase (article the) (noun student)) (verb-phrase (verb studies) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun professor)))))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(sentence (simple-noun-phrase (article the) (noun student)) (verb-phrase (verb studies) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun cat)))))

; 4.50
; ========================================================================
;; This code *should* work but again I can't get the amb-evaluator to
;; play ball - whenever I try and call (sort) inside of it, it says:

;;; Amb-Eval input:
;; (sort (list 1 2 3 4)
;;       (lambda (x y)
;; 	(eq? 0 (random 2))))
;;; Starting a new problem
;The object (procedure (x y) #[compound-procedure 2] (((false true car cdr cons null? list memq member not + - * = > >= abs remainder integer? sqrt eq? sort random) #f #t (primitive #[compiled-procedure 3 ("list" #x1) #x3 #x4d2a2]) (primitive #[compiled-procedure 4 ("list" #x2) #x3 #x4d29b]) (primitive #[compiled-procedure 5 ("list" #x3) #x3 #x4d295]) (primitive #[compiled-procedure 6 ("list" #x5) #x3 #x4d286]) (primitive #[compiled-procedure 7 ("list" #x9) #x3 #x4d269]) (primitive #[compiled-procedure 8 ("list" #x8a) #x3 #x4c6da]) (primitive #[compiled-procedure 9 ("list" #x8c) #x3 #x4c6a5]) (primitive #[compiled-procedure 10 ("boole" #x1) #x3 #x48a74]) (primitive #[arity-dispatched-procedure 11]) (primitive #[arity-dispatched-procedure 12]) (primitive #[arity-dispatched-procedure 13]) (primitive #[arity-dispatched-procedure 14]) (primitive #[arity-dispatched-procedure 15]) (primitive #[arity-dispatched-procedure 16]) (primitive #[compiled-procedure 17 ("arith" #xb6) #x3 #x37924]) (primitive #[compiled-procedure 18 ("arith" #x10b) #x3 #x371a8]) (primitive #[compiled-procedure 19 ("arith" #x9d) #x3 #x37da9]) (primitive #[compiled-procedure 20 ("arith" #xdc) #x3 #x3750e]) (primitive #[compiled-procedure 21 ("global" #x14) #x3 #x480ab]) (primitive #[compiled-procedure 22 ("msort" #x1) #x3 #x47ae1]) (primitive #[compiled-procedure 23 ("random" #x7) #x3 #x4b5ea])))) is not applicable.

;; And then kills my evaluator subprocess.

(load "book_code/ch4-ambeval.scm")
(define the-global-environment (setup-environment))
(driver-loop)

(define (random-sort l)
  (sort l
        (lambda (x y)
          (eq? 0 (random 2)))))

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (random-sort (cdr exp)))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
	((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

; 4.51
; ========================================================================
;; Okay I finally managed to get the amb-evaluator to recognize my
;; modified definitions of (eval)!  It is all in the order you load your
;; code into the environment.
;; First load book_code/ch4-ambeval.scm
;; Then load the custom (analyze) and operators below
;; Then start the (driver-loop)
;; Then load the definitions like (require) and (an-element-of)
;; Finally you can run your program that uses permanent-set!
(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
	       (set-variable-value! var val env)
	       (succeed 'ok fail2))
	     fail))))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
	((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define count 0)
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))
(define (require p) (if (not p) (amb)))

(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

;;; Starting a new problem
;;; Amb-Eval value:
;; (a b 3)

;;If we didn't use permanent-set! the results would be
;;(a b 1) (a c 1)



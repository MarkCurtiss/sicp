; 2.17
; ========================================================================
(define (last-pair items)
  (list (list-ref items (- (length items) 1))))

; 2.18
; ========================================================================
(define (reverse items)
  (define (iter count reversed-list)
    (if (= (length reversed-list) (length items))
	reversed-list
	(iter
	 (+ count 1)
	 (cons (list-ref items (+ 0 count)) reversed-list))))

  (iter 0 '()))

;; 1 ]=> (reverse (list 1 4 9 16 25))
;; Value 10: (25 16 9 4 1)

; 2.19
; ========================================================================
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? coin-values)
  (null? coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;; 1 ]=> (cc 100 us-coins)
;; Value: 292

;; No it doesn't matter what order the coins are in.  Each denomination
;; is still considered and evaluated as cc steps through the list.  And
;; the evaluation of any given denomination is completely independent
;; of the evaluation of the next denomination.

;; 2 error> (define backwards-coins (list 10 1 25 50 5))
;; Value: backwards-coins
;; 2 error> (cc 100 backwards-coins)
;; Value: 292

; 2.20
; ========================================================================
(define (same-parity x . items)
  (define (has-parity? y)
    (or (and (odd? x) (odd? y))
	(and (even? x) (even? y))))

  (define (iter items results)
      (cond ((null? items) results)
	    ((has-parity? (car items)) (iter (cdr items) (append results (list (car items)))))
	    (else (iter (cdr items) results))))

  (iter (cons x items) '()))

;; 1 ]=> (same-parity 1 2 3 4 5 6 7)
;; Value 25: (1 3 5 7)
;; 1 ]=> (same-parity 2 3 4 5 6 7)
;; Value 26: (2 4 6)

; 2.21
; ========================================================================
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

;; 1 ]=> (square-list (list 1 2 3 4))
;; Value 29: (1 4 9 16)

(define (square-list items)
  (map square items))

;; 1 ]=> (square-list (list 1 2 3 4))
;; Value 32: (1 4 9 16)

; 2.22
; ========================================================================
;; He is building up 'answer' by prepending to it while iterating over
;; 'items'. What did he think was going to happen??

;; His second implementation is going to put a nil at the front of the list.
;; Also, it's going to return nested list as the first argument to cons is
;; always going to be a list

; 2.23
; ========================================================================
(define (my-for-each func items)
  (define (iter things)
    (cond ((null? things) true)
	  (else
	   (func (car things))
	   (iter (cdr things)))
	  ))

  (iter items))


;; 1 ]=> (my-for-each (lambda (x) (newline) (display x)) (list 57 321 88))
;; 57
;; 321
;; 88
;; Value: #t

; 2.24
; ========================================================================

;; 1 ]=> (list 1 (list 2 (list 3 4)))
;; Value 36: (1 (2 (3 4)))

;; 1
;;  \
;;   \
;;    2
;;   / \
;;  /   \
;; 3     4

; 2.25
; ========================================================================
;; (cdr (car (cdr (cdr
;;  (list 1 3 (list 5 7) 9)
;; ))))
;; Value 44: (7)

;; (car (car
;;   (list (list 7))
;; ))
;; Value: 7

;; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr
;;   (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))
;; ))))))))))))
;; Value: 7

; 2.26
; ========================================================================
(define x (list 1 2 3))
(define y (list 4 5 6))

;; 1 ]=> (append x y)
;; Value 56: (1 2 3 4 5 6)

;; 1 ]=> (cons x y)
;; Value 57: ((1 2 3) 4 5 6)

;; 1 ]=> (list x y)
;; Value 58: ((1 2 3) (4 5 6))


; 2.27
; ========================================================================
(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse items)
  (define (iter things answer)
    (cond ((null? things) answer)
	  ((pair? (car things)) (iter (cdr things) (cons (reverse (car things)) answer)))
	  (else (iter (cdr things) (append answer (car things))))))

  (iter items '()))

;; 1 ]=> (deep-reverse x)
;; Value 72: ((4 3) (2 1))

; 2.28
; ========================================================================
(define x (list (list 1 2) (list 3 4)))

(define (fringe tree)
  (cond ((null? tree) '())
	((pair? tree) (append (fringe (car tree)) (fringe (cdr tree))))
	(else (list tree))))

;; 1 ]=> (fringe x)
;; Value 83: (1 2 3 4)
;; 1 ]=> (fringe (list x x))
;; Value 84: (1 2 3 4 1 2 3 4)

; 2.29
; ========================================================================
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a.
(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

;; b.
(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
	(if (pair? structure)
	    (+ (branch-weight (left-branch structure))
	       (branch-weight (right-branch structure)))
	    structure)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))


(define test-mobile (make-mobile (make-branch 1 2) (make-branch 3 (make-mobile (make-branch 4 5) (make-branch 6 7)))))

;; 1 ]=> (total-weight test-mobile)
;; Value: 14

;; c.
(define (branch-torque branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure)
	(* (branch-length branch)
	   (branch-torque (left-branch structure))
	   (branch-torque (right-branch structure)))
	(* (branch-length branch) structure))))

(define balanced-test-mobile (make-mobile (make-branch 10 12) (make-mobile 2
									   (make-mobile (make-branch 1 5) (make-branch 2 6)))))
(define (balanced-mobile? mobile)
  (= (branch-torque (left-branch mobile))
     (branch-torque (right-branch mobile))))

;; 1 ]=> (balanced-mobile? test-mobile)
;; Value: #f
;; 1 ]=> (balanced-mobile? balanced-test-mobile)
;; Value: #t

;; d.
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

;; None at all!  A (cons left right) is accessed and represented the same way as (list left right)

;; 1 ]=> (balanced-mobile? balanced-test-mobile)
;; Value: #t
;; 1 ]=> (balanced-mobile? test-mobile)
;; Value: #f
;; 1 ]=> (total-weight balanced-test-mobile)
;; Value: 23

; 2.30
; ========================================================================
(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree-direct tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else (cons (square-tree-direct (car tree)) (square-tree-direct (cdr tree))))))

;; 1 ]=> (square-tree-direct test-tree)
;; Value 124: (1 (4 (9 16) 25) (36 49))

(define (square-tree tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (square-tree subtree)
	     (square subtree)))
       tree))

;; 1 ]=> (square-tree test-tree)
;; Value 126: (1 (4 (9 16) 25) (36 49))

; 2.31
; ========================================================================
(define (tree-map function tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (tree-map function subtree)
	     (function subtree)))
       tree))

(define (square-tree tree) (tree-map square tree))

;; 1 ]=> (square-tree test-tree)
;; Value 127: (1 (4 (9 16) 25) (36 49))

; 2.32
; ========================================================================
;; (subsets (list 1 2 3))
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; 1 ]=> (subsets (list 1 2 3))
;; Value 130: (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;; This combines the first element of every set with every possible combination
;; of subsets.

; 2.33
; ========================================================================
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (sequence-map p sequence)
  (accumulate
   (lambda (x y)
     (cons (p x) y))
   '()
   sequence))

;; 1 ]=> (sequence-map (lambda (x) (square x)) (list 2 3 4 5))
;; Value 134: (4 9 16 25)

(define (sequence-append seq1 seq2)
  (accumulate cons seq2 seq1))

;; 1 ]=> (sequence-append (list 1 2 3) (list 4 5 6))
;; Value 136: (1 2 3 4 5 6)

(define (sequence-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; 1 ]=> (sequence-length (list 8 4 2 7 3))
;; Value: 5

; 2.34
; ========================================================================
(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ (* x higher-terms) this-coeff)
     )
   0
   coefficient-sequence))

;; (horner-eval 2 (list 1 3 0 5 0 1))
;; Should be 79

;; 1 ]=> (horner-eval 2 (list 1 3 0 5 0 1))
;; Value: 79

; 2.35
; ========================================================================
(define (count-leaves t)
  (accumulate
   +
   0
   (map (lambda (x) 1) (fringe t))))

(define leafy-tree (cons (list 1 2) (list 3 4)))
;;Value 141: ((1 2) 3 4)

;; 1 ]=> (count-leaves leafy-tree)
;; Value: 4
;; 1 ]=> (count-leaves (list leafy-tree leafy-tree))
;; Value: 8

; 2.36
; ========================================================================
(define test-sequence (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

;; 1 ]=> (accumulate-n + 0 test-sequence)
;; Value 161: (22 26 30)

; 2.37
; ========================================================================
(define test-matrix (list (list 1 2) (list 3 4)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; 1 ]=> (dot-product (list 1 2) (list 3 4))
;; Value: 11

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

;; 1 ]=> (matrix-*-vector test-matrix (list 5 6))
;; Value 162: (17 39)

(define (transpose mat)
  (accumulate-n cons '() mat))

;; 1 ]=> (transpose test-matrix)
;; Value 163: ((1 3) (2 4))
;; 1 ]=> (transpose (list (list 1 2 3) (list 4 5 6)))
;; Value 164: ((1 4) (2 5) (3 6))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;; 1 ]=> (matrix-*-matrix test-matrix test-matrix)
;; Value 166: ((7 10) (15 22))

; 2.38
; ========================================================================
;; 1 ]=> (fold-right / 1 (list 1 2 3))
;; Value: 3/2

;; 1 ]=> (fold-left / 1 (list 1 2 3))
;; Value: 1/6

;; 1 ]=> (fold-right list '() (list 1 2 3))
;; Value 168: (1 (2 (3 ())))

;; 1 ]=> (fold-left list '() (list 1 2 3))
;; Value 169: (((() 1) 2) 3)

;; An operation has to have the commutative property in order for fold-left
;; and fold-right to produce the same.  This is because the operation
;; can't care that the order of operations are going to be different.

; 2.39
; ========================================================================
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

;; 1 ]=> (reverse (list 1 4 9 16 25))
;; Value 175: (25 16 9 4 1)

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

;; 1 ]=> (reverse (list 1 4 9 16 25))
;; Value 178: (25 16 9 4 1)

; 2.40
; ========================================================================
(load "../prime.scm")

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

; 2.41
; ========================================================================
(define (unique-triples n)
  (define (ordered? triple)
    (< (car triple) (cadr triple) (cadr (cdr triple))))

  (define (unique? triple)
    (not (= (car triple) (cadr triple) (cadr (cdr triple)))))

  (filter
   ordered?
   (filter
    unique?
    (flatmap
     (lambda (i)
       (flatmap (lambda (j)
		  (map (lambda (k) (list i j k))
		       (enumerate-interval 1 n)
		       ))
		(enumerate-interval 1 n))
       )
     (enumerate-interval 1 n)))))


(define (triples-up-to-n-what-sum-to-s n s)
  (define (sum-to-s? triple)
    (= (+ (car triple) (cadr triple) (cadr (cdr triple))) s))

  (filter
   sum-to-s?
   (unique-triples n)))


;; 1 ]=> (triples-up-to-n-what-sum-to-s 3 4)
;; Value: ()

;; 1 ]=> (triples-up-to-n-what-sum-to-s 3 6)
;; Value 212: ((1 2 3))

;; 1 ]=> (triples-up-to-n-what-sum-to-s 8 9)
;; Value 213: ((1 2 6) (1 3 5) (2 3 4))

;; 1 ]=> (triples-up-to-n-what-sum-to-s 10 15)
;; Value 214: ((1 4 10) (1 5 9) (1 6 8) (2 3 10) (2 4 9) (2 5 8) (2 6 7) (3 4 8) (3 5 7) (4 5 6))

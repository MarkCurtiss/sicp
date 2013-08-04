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


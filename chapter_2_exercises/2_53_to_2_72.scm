; 2.53
; ========================================================================
;; My guess: (a b c)
;; 1 ]=> (list 'a 'b 'c)
;; Value 12: (a b c)

;; My guess: ((george))
;; 1 ]=> (list (list 'george))
;; Value 13: ((george))

;; My guess: (y1 y2)
;; 1 ]=> (cdr '((x1 x2) (y1 y2)))
;; Value 14: ((y1 y2))

;; My guess: y1
;; 1 ]=> (cadr '((x1 x2) (y1 y2)))
;; Value 15: (y1 y2)
;; Dang man why does cdr give you a nested list?
;; Oh I guess because cdr always returns a list and the contents are also
;; a list.

;; My guess: #f
;; 1 ]=> (pair? (car '(a short list)))
;; Value: #f

;; My guess: #f
;; 1 ]=> (memq 'red '((red shoes) (blue socks)))
;; Value: #f

;; My guess: (red shoes blue socks)
;; 1 ]=> (memq 'red '(red shoes blue socks))
;; Value 19: (red shoes blue socks)

; 2.54
; ========================================================================
;; (equal? '(this is a list) '(this is a list))
;; #t

;; (equal? '(this is a list) '(this (is a) list))
;; #f

(define (my-equal? a b)
  (cond ((and (not (pair? a))
	      (not (pair? b))) (eq? a b))
	((and (pair? a)
	      (pair? b)) (and (eq? (car a) (car b))
			      (my-equal? (cdr a) (cdr b))))
	(else #f)))


;; 1 ]=> (my-equal? '(this is a list) '(this is a list))
;; Value: #t
;; 1 ]=> (my-equal? '(this is a list) '(this (is a) list))
;; Value: #f

; 2.55
; ========================================================================
;; What they typed is logically equivalent to:

;; (quote (quote abracadabra))

;; Uh and the (car) of that is clearly gonna be the operator 'quote'

; 2.56
; ========================================================================
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (sum? x)  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (base exp)
	  (make-expt (base exp)
		     (- (power exp) 1))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**))
)
(define (base x) (cadr x))
(define (power x) (caddr x))
(define (make-expt base power)
    (cond ((=number? power 0) 1)
	  ((=number? power 1) base)
	  ((and (number? base) (number? power)) (expt base power))
	  (else (list '** base power)))
)

;; 1 ]=> (make-expt 2 0)
;; Value: 1
;; 1 ]=> (make-expt 8 1)
;; Value: 8
;; 1 ]=> (make-expt 2 5)
;; Value: 32

;; 1 ]=> (deriv (make-expt 'a 8) 'a)
;; Value 37: (* a (** a 7))

; 2.57
; ========================================================================
(define (addend s) (cadr s))
(define (augend s)
  (cond ((> (length s) 3) (cons '+ (cddr s)))
	(else (caddr s))))

(define (multiplier p) (cadr p))
(define (multiplicand p)
  (cond ((> (length p) 3) (cons '* (cddr p)))
	(else (caddr p))))

;; 1 ]=> (deriv '(* x y (+ x 3)) 'x)
;; Value 165: (+ (* x y) (* y (+ x 3)))

; 2.58
; ========================================================================
;;a.
(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(define (sum? x)  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

;; 1 ]=> (deriv '(x + (3 * (x + (y + 2)))) 'x)
;; Value: 4

;;b. (deriv '(x + 3 * (x + y + 2)) 'x)
;;should be 4
;; we need
;; 2 error> (augend '(x + y + 2))
;; to return
;; Value 170: (y + 2)

; 2.59
; ========================================================================
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 (filter (lambda (x) (not (element-of-set? x set1))) set2)))

;; 1 ]=> (union-set '(1 2 3 4) '(2 4 6 8))
;; Value 179: (1 2 3 4 6 8)

; 2.60
; ========================================================================
(define ts1 '(1 1 2 3 4))
(define ts2 '(2 4 6 8 8))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; 1 ]=> (element-of-set? 2 ts1)
;; Value: #t
;; 1 ]=> (element-of-set? 8 ts1)
;; Value: #f
;; This is the same efficiency.

(define (adjoin-set x set)
  (cons x set))

;; 1 ]=> (adjoin-set 8 ts1)
;; Value 180: (8 1 1 2 3 4)
;; This is now a constant time operation!

(define (union-set set1 set2)
  (append set1 set2))

;; 1 ]=> (union-set ts1 ts2)
;; Value 181: (1 1 2 3 4 2 4 6 8 8)
;; This is O(n) whereas before it was O(n^2)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

;; 1 ]=> (intersection-set ts1 ts2)
;; Value 188: (2 4)
;; This is O(n^2) again.

;; This would be good for applications that have to be fast
;; at adding to sets or union-ing sets together.  This could
;; have been good for our n-queens representation since we were
;; constantly adjoining sets.

; 2.61
; ========================================================================
(define ts1 '(1 2 3 4))
(define ts2 '(2 4 6 8))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (let ((head (car set)))
  (cond  ((= x head) set)
	 ((< x head) (cons x set))
	 (else (cons head (adjoin-set x (cdr set)))))))

;; 1 ]=> (adjoin-set 5 ts2)
;; Value 205: (2 4 5 6 8)

; 2.62
; ========================================================================
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1)) (x2 (car set2)))
	   (cond ((= x1 x2)
		  (cons x1 (union-set (cdr set1) (cdr set2))))
		 ((< x1 x2)
		  (cons x1 (union-set (cdr set1) set2)))
		 ((> x1 x2)
		  (cons x2 (union-set set1 (cdr set2)))))))))

;; 1 ]=> (union-set ts1 ts2)
;; Value 211: (1 2 3 4 6 8)

; 2.63
; ========================================================================
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;;     7
;;    / \
;;   3   9
;;  / \   \
;; 1   5   11
(define tree1 (make-tree 7
			 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
			 (make-tree 9 '() (make-tree 11 '() '()))))

;;   3
;;  / \
;; 1   7
;;    / \
;;   5   9
;;        \
;;         11

(define tree2 (make-tree 3
			 (make-tree 1 '() '())
			 (make-tree 7 (make-tree 5 '() '()) (make-tree 9 '() (make-tree 11 '() '())))))

;;     5
;;    / \
;;   3   9
;;  /   / \
;; 1   7  11

(define tree3 (make-tree 5
			 (make-tree 3 (make-tree 1 '() '()) '())
			 (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;;a.
;; Yes.
;; 1 ]=> (tree->list-2 tree2)
;; Value 227: (1 3 5 7 9 11)

;;b.
;; No.

; 2.64
; ========================================================================
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;;a.
;; (partial-tree) constructs a binary tree of size n from the elements you pass in.
;; Since you know that the list is ordered, you can treat the center of the list as the root
;; node and the left and right halves of the list as the left and right subtrees.
;; Thus, (partial-tree) divides the elements list in half recursively until it runs out of sublists,
;; then uses (make-tree) to construct balanced binary trees from these sublists with the middle
;; element of the list at the root.  So (partial-tree '(1 3 4 7 9 11), 3) would return:
;;   3
;;  / \
;; 1   5
;; The result of (list->tree '(1 3 5 7 9 11)) is:
;;      5
;;     / \
;;    /   \
;;   1     9
;;  / \   / \
;; ()  3 7  11
;;b.
;;Since (partial-tree) visits every element in the tree only once, it has O(n) growth.

; 2.65
; ========================================================================
(define tree1 (list->tree '(1 2 3 4)))
(define tree2 (list->tree '(2 4 6 8)))

(define (union-set-tree tree1 tree2)
  (let ((list1 (tree->list-1 tree1))
	(list2 (tree->list-1 tree2)))
    (list->tree (union-set list1 list2))))

;; 1 ]=> (union-set-tree tree1 tree2)
;; Value 232: (3 (1 () (2 () ())) (6 (4 () ()) (8 () ())))
;;     3
;;    / \
;;   /   \
;;  1     6
;;   \   / \
;;    2 4   8

(define (intersection-set-tree tree1 tree2)
  (let ((list1 (tree->list-1 tree1))
	(list2 (tree->list-1 tree2)))
    (list->tree (intersection-set list1 list2))))

;; 1 ]=> (intersection-set-tree tree1 tree2)
;; Value 233: (2 () (4 () ()))
;;  2
;;   \
;;    4

; 2.66
; ========================================================================
(define (lookup given-key records-as-tree)
  (cond ((null? records-as-tree) false)
	((equal? given-key (entry records-as-tree)) (entry records-as-tree))
	((< given-key (entry records-as-tree)) (lookup given-key (left-branch records-as-tree)))
	(else (lookup given-key (right-branch records-as-tree)))))

;; 1 ]=> (lookup 6 tree2)
;; Value: 6
;; 1 ]=> (lookup 9 tree2)
;; Value: #f

; 2.67
; ========================================================================
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;;                       a d     a b   b   c     a
;; 1 ]=> (decode sample-message sample-tree)
;; Value 243: (a d a b b c a)

; 2.68
; ========================================================================
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (element-of-set? x set)
    (cond ((null? set) false)
	  ((equal? x (car set)) true)
	  (else (element-of-set? x (cdr set)))))

  (define (iter symbol tree encoding)
    (cond ((null? tree)
	   (error "symbol" symbol "is not in the tree"))
	  ((leaf? tree)
	   (if (equal? (symbol-leaf tree) symbol)
	       encoding
	       (error "symbol" symbol "made it to a leaf node" tree "without finding a match")))
	  ((element-of-set? symbol (symbols (left-branch tree)))
	   (iter symbol (left-branch tree) (append encoding '(0))))
	  (else
	   (iter symbol (right-branch tree) (append encoding '(1))))
    ))

  (iter symbol tree '()))

;; 1 ]=> (encode '(a d a b b c a) sample-tree)
;; Value 261: (0 1 1 0 0 1 0 1 0 1 1 1 0)
;; 1 ]=> (equal? (encode '(a d a b b c a) sample-tree) sample-message)
;; Value: #t

; 2.69
; ========================================================================
(define test-pairs '((A 4) (B 2) (C 1) (D 1)))
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (define (iter tree leaf-set)
    (cond ((null? leaf-set)
	   tree)
	  ((null? tree)
	   (iter (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set)))
	  (else
	   (iter (make-code-tree (car leaf-set) tree) (cdr leaf-set)))))

  (iter '() leaf-set))

;; 1 ]=> (generate-huffman-tree test-pairs)
;; Value 291: ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)

; 2.1
; ========================================================================
(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((and (< n 0) (< d 0)) (cons (/ (- n) g) (/ (- d) g)))
	  ((or (< n 0) (< d 0)) (cons (/ (abs n) g) (/ (- (abs d)) g)))
	  (else (cons (/ n g) (/ d g))))))

; 2.2
; ========================================================================
(define (average x y)
  (/ (+ x y) 2))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-point
   (average (x-point (start-segment segment)) (x-point (end-segment segment)))
   (average (y-point (start-segment segment)) (y-point (end-segment segment)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;1 ]=> (define seg (make-segment (make-point 0 0) (make-point 4 4)))
;Value: seg
;1 ]=> (print-point (midpoint-segment seg))
;(2,2)
;Unspecified return value

; 2.3
; ========================================================================
(define (length-segment segment)
  (sqrt (+
	 (square (abs (- (x-point (end-segment segment)) (x-point (start-segment segment)))))
	 (square (abs (- (y-point (end-segment segment)) (y-point (start-segment segment)))))
	 )))

(define (west-side rectangle)
  (car (car rectangle)))

(define (north-side rectangle)
  (cdr (car rectangle)))

(define (east-side rectangle)
  (car (cdr rectangle)))

(define (south-side rectangle)
  (cdr (cdr rectangle)))

(define (length-rect rectangle)
  (length-segment (north-side rectangle)))

(define (height-rect rectangle)
  (length-segment (west-side rectangle)))

(define (make-rect ll ul ur lr)
  (cons (cons (make-segment ll ul)
	      (make-segment ul ur))
	(cons (make-segment ur lr)
	      (make-segment ll lr))
	))

(define (print-rect rectangle)
  (print-point (start-segment (west-side rectangle)))
  (print-point (end-segment (west-side rectangle)))
  (print-point (start-segment (east-side rectangle)))
  (print-point (end-segment (south-side rectangle))))

(define (perimiter-rect rectangle)
  (+
   (* 2 (length-rect rectangle))
   (* 2 (height-rect rectangle))
   ))

;1 ]=> (define rect (make-rect (make-point 0 0) (make-point 0 8) (make-point 16 8) (make-point 16 0)))
;Value: rect
;1 ]=> (perimiter-rect rect)
;Value: 48

(define (area-rect rectangle)
  (* (length-rect rectangle)
     (height-rect rectangle)))

;1 ]=> (area-rect rect)
;Value: 128

(define (make-rect x y length height)
  (cons (cons (make-segment (make-point x y) (make-point x (+ height y)))
	      (make-segment (make-point x (+ height y)) (make-point (+ length x) (+ height y))))
	(cons (make-segment (make-point (+ length x) (+ height y)) (make-point (+ length x) y))
	      (make-segment (make-point x y) (make-point (+ length x) y)))
	      ))

;1 ]=> (define rect (make-rect 0 0 16 8))
;Value: rect
;1 ]=> (perimiter-rect rect)
;Value: 48
;1 ]=> (area-rect rect)
;Value: 128

; 2.4
; ========================================================================
(define (cons-proc x y)
  (lambda (m) (m x y)))

(define (car-proc z)
  (z (lambda (p q) p)))

;(car-proc (cons-proc 3 8))
;(car-proc (lamdba (m) (m 3 8)))
;((lambda (m) (m 3 8)) (lambda (p q) p))
;((lambda (p q) p) 3 8)
;(lambda (3 8) 3)
;3

(define (cdr-proc z)
  (z (lambda (p q) q)))

; 2.5
; ========================================================================
(define (cons-num x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (find-even-divisor x base)
  (define (iter prev-guess guess)
    (if (integer? (/ x (expt base guess)))
	(iter guess (+ 1 guess))
	prev-guess))

  (iter 0 1))

(define (log2 x)
  (/
   (log x)
   (log 2)))

(define (car-num pair)
  (log2 (/ pair (expt 3 (find-even-divisor pair 3)))))

;1 ]=> (car-num (cons-num 3 8))
;Value: 3.

(define (log3 x)
  (/
   (log x)
   (log 3)))

(define (cdr-num pair)
  (log3 (/ pair (expt 2 (find-even-divisor pair 2)))))

;1 ]=> (cdr-num (cons-num 3 8))
;Value: 8.

; 2.6
; ========================================================================
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;(add-1 zero)
;(add-1 (lambda (f) (lambda (x) x)))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x))) f) x)))
;(lambda (f) (lambda (x) (f (x x))))
;(lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (church-plus a b)
  (lambda (f) (lambda (x) (a b))))

; 2.7
; ========================================================================
(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (print-interval interval)
  (newline)
  (display "[")
  (display (lower-bound interval))
  (display "..")
  (display (upper-bound interval))
  (display "]"))

;1 ]=> (add-interval (make-interval 86 14) (make-interval 8 42))
;Value 2: (22 . 128)

; 2.8
; ========================================================================
; The largest difference between two intervals would be the difference
; between the smallest lower-bound and the largest upper-bound.
; The smallest difference would be the difference between the largest
; lower-bound and the smallest upper-bound.

(define (sub-interval x y)
  (make-interval
   (min
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y)))
   (max
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y)))
   ))

;1 ]=> (print-interval (sub-interval (make-interval 8 186) (make-interval 72 237)))
;[-229..114]
;1 ]=> (print-interval (sub-interval (make-interval 8 186) (make-interval 987 4200)))
;[-4192..-801]

; 2.9
; ========================================================================
; Per our instructor, we are skipping this one.

; 2.10
; ========================================================================
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error "Cannot divide by an interval that spans zero!" y)
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

;1 ]=> (div-interval (make-interval 8 186) (make-interval 58 -4))
;Cannot divide by an interval that spans zero! (58 . -4)
;1 ]=> (div-interval (make-interval 8 186) (make-interval -58 -4))
;Value 10: (-46.5 . -.13793103448275862)

; 2.11
; ========================================================================
(define (mul-interval x y)
  (let ((x1 (lower-bound x))
	(x2 (upper-bound x))
	(y1 (lower-bound y))
	(y2 (upper-bound y)))

    (cond ((and (positive? x1)
		(positive? y1)
		(positive? x2)
		(positive? y2))
	   (make-interval (* x1 y1) (* x2 y2)))

	  ((and (negative? x1)
		(positive? y1)
		(positive? x2)
		(positive? y2))
	   (make-interval (* x1 y2) (* x2 y2)))

	  ((and (negative? x1)
		(positive? y1)
		(negative? x2)
		(positive? y2))
	   (make-interval (* x1 y2) (* x2 y1)))

	  ((and (positive? x1)
		(negative? y1)
		(positive? x2)
		(positive? y2))
	   (make-interval (* x2 y1) (* x2 y2)))

	  ((and (positive? x1)
		(negative? y1)
		(positive? x2)
		(negative? y2))
	   (make-interval (* x2 y1) (* x1 y2)))

	  ((and (negative? x1)
		(negative? y1)
		(positive? x2)
		(positive? y2))
	   (make-interval (* x2 y1) (* x2 y2)))

	  ((and (negative? x1)
		(negative? y1)
		(negative? x2)
		(positive? y2))
	   (make-interval (* x1 y2) (* x1 y1)))

	  ((and (negative? x1)
		(negative? y1)
		(negative? x2)
		(negative? y2))
	   (make-interval (* x2 y2) (* x1 y1)))

	  ((and (negative? x1)
		(negative? y1)
		(positive? x2)
		(negative? y2))
	   (make-interval (* x2 y1) (* x1 y1)))
	  )))
;1 ]=> (print-interval (mul-interval (make-interval 8 186) (make-interval 72 237)))
;[576..44082]
;1 ]=> (print-interval (mul-interval (make-interval -8 186) (make-interval 72 237)))
;[-1896..44082]
;1 ]=> (print-interval (mul-interval (make-interval -8 -186) (make-interval 72 237)))
;[-44082..-576]
;1 ]=> (print-interval (mul-interval (make-interval 8 186) (make-interval -72 237)))
;[-13392..44082]
;1 ]=> (print-interval (mul-interval (make-interval 8 186) (make-interval -72 -237)))
;[-44082..-576]
;1 ]=> (print-interval (mul-interval (make-interval -8 186) (make-interval -72 237)))
;[-13392..44082]
;1 ]=> (print-interval (mul-interval (make-interval -8 -186) (make-interval -72 237)))
;[-44082..13392]
;1 ]=> (print-interval (mul-interval (make-interval -8 -186) (make-interval -72 -237)))
;[576..44082]
;1 ]=> (print-interval (mul-interval (make-interval -8 186) (make-interval -72 -237)))
;[-44082..1896]

; 2.12
; ========================================================================
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percent)
  (make-interval
   (- center (* center percent))
   (+ center (* center percent))))

;1 ]=> (print-interval (make-center-percent 3.5 .10))
;[3.15..3.85]

(define (percent interval)
  (/ (- (center interval) (lower-bound interval)) (center interval)))

;1 ]=> (percent (make-center-percent 3.5 .10))
;Value: .10000000000000002

; 2.13
; ========================================================================
; Per our instructor, we are skipping this.

; 2.14
; ========================================================================
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define A (make-interval 3.15 3.65))
(define B (make-interval 4.18 6.42))

;; 1 ]=> (print-interval (par1 A B))
;; [1.3075471698113206..3.1968622100954978]
;; 1 ]=> (print-interval (par2 A B))
;; [1.7963165075034107..2.327010923535253]

;; 1 ]=> (print-interval (div-interval A A))
;; [.8630136986301369..1.1587301587301586]
;; 1 ]=> (center (div-interval A A))
;; Value: 1.0108719286801477
;; 1 ]=> (percent (div-interval A A))
;; Value: .14626801462680147

;; 1 ]=> (print-interval (div-interval A B))
;; [.4906542056074766...8732057416267943]
;; 1 ]=> (center (div-interval A B))
;; Value: .6819299736171355
;; 1 ]=> (percent (div-interval A B))
;; Value: .2804918032786886

; 2.15
; ========================================================================
;; No I don't think so!  I think the issue is that these expressions are
;; algebraically equivalent *assuming numbers* and not some compound user
;; defined objects.  It is expecting a definition of multiplication that we
;; are not adhering to.

;; As an example, what would it mean if we passed in strings to these
;; functions?  What does it mean to multiply and divide two strings?
;; We wouldn't expect the functions to produce equivalent results then.

; 2.16
; ========================================================================
;; See my answer to 2.15.  I don't think I could devise an interval
;; arithmetic package free of this shortcoming unless I rigorously proved
;; all sorts of algebraic properties on interval operations first.
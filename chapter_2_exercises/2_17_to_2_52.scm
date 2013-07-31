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
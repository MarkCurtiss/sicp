; 3.63
; ========================================================================
;; Louis Reasoner's implementation will recalculate elements of the stream
;; while Alyssa P. Hacker's takes advantage of the memoization that (delay)
;; gave us.  If (delay) was implemented sans memoization there'd be no
;; difference in performance.

; 3.64
; ========================================================================
(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (first-n-elements-of-stream stream n)
  (define (iter list s index)
    (if (= index n)
	list
	(iter (cons (stream-car s) list)
	      (stream-cdr s)
	      (+ index 1))))

  (reverse (iter '() stream 0)))

(define (stream-limit stream tolerance)
  (define (iter last-guess s)
    (let ((guess (stream-car s)))
      (if (and last-guess (< (abs (- last-guess guess)) tolerance))
	  guess
	  (iter guess (stream-cdr s)))))
  (iter #f (stream-cdr stream))
  )

; 3.65
; ========================================================================
(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (ln-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (ln-summands (+ n 1)))))
(define ln-stream
  (partial-sums (ln-summands 1)))

;; 1 ]=> (first-n-elements-of-stream ln-stream 40)
;; Value 6: (1. .5 .8333333333333333 .5833333333333333 .7833333333333332 .6166666666666666 .7595238095238095 .6345238095238095 .7456349206349207 .6456349206349207 .7365440115440116 .6532106782106782 .7301337551337552 .6587051837051838 .7253718503718505 .6628718503718505 .7216953797836152 .6661398242280596 .718771403175428 .6687714031754279 .7163904507944756 .6709359053399302 .7144141662094954 .6727474995428288 .7127474995428288 .6742859610812904 .7113229981183273 .6756087124040416 .7100914710247312 .6767581376913979 .7090162022075269 .6777662022075269 .7080692325105572 .6786574678046748 .7072288963761034 .6794511185983256 .7064781456253526 .6801623561516684 .7058033817926941 .6808033817926941)
;; This implementation is converging "slowly".

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define eulerized-ln
  (euler-transform ln-stream))

;; 1 ]=> (first-n-elements-of-stream eulerized-ln 40)
;; Value 7: (.7 .6904761904761905 .6944444444444444 .6924242424242424 .6935897435897436 .6928571428571428 .6933473389355742 .6930033416875522 .6932539682539683 .6930657506744464 .6932106782106783 .6930967180967181 .6931879423258734 .6931137858557215 .6931748806748808 .6931239512121866 .6931668512550866 .6931303775344023 .693161647077867 .6931346368409872 .6931581275621524 .6931375704648145 .6931556628081349 .6931396564055738 .6931538856095922 .6931411799365091 .6931525720531644 .6931423184823584 .6931515803143488 .6931431863345111 .6931508175921423 .6931438593762288 .6931502214278632 .6931443893338499 .6931497487353119 .6931448122920193 .6931493691386814 .6931451539445929 .6931490608050398 .6931454329240487)
;; This implementation is converging faster - the actual value is
;; 0.693147180559945...
;; and we closing in on that after 40 elements.

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define accelerated-ln
  (accelerated-sequence euler-transform ln-stream))

;; 1 ]=> (first-n-elements-of-stream accelerated-ln 40)
;; Value 8: (1. .7 .6932773109243697 .6931488693329254 .6931471960735491 .6931471806635636 .6931471805604039 .6931471805599445 .6931471805599427 .6931471805599454 #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN] #[NaN])
;; This one converged "quickly."

; 3.66
; ========================================================================
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; 1 ]=> (first-n-elements-of-stream (pairs integers integers) 40)
;; Value 11: ((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (1 5) (2 4) (1 6) (3 4) (1 7) (2 5) (1 8) (4 4) (1 9) (2 6) (1 10) (3 5) (1 11) (2 7) (1 12) (4 5) (1 13) (2 8) (1 14) (3 6) (1 15) (2 9) (1 16) (5 5) (1 17) (2 10) (1 18) (3 7) (1 19) (2 11) (1 20) (4 6) (1 21))
;; The pairs are entering the stream as
;; (
 ;;  (car S0, cdr T)
;;  (cdr S, cdr T)
;; )

;; So about 200 pairs will precede (1, 100).  (The actual number is 197).

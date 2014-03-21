; 3.50
; ========================================================================
(define (multi-stream-map proc . argstreams)
  (if (null? (stream-car (car argstreams)))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply multi-stream-map
              (cons proc (map stream-cdr argstreams))))))

; 3.51
; ========================================================================
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

;; 1 ]=> (define x (stream-map show (stream-enumerate-interval 0 10)))
;; 0
;; ;Value: x

;; 1 ]=> (stream-ref x 5)
;; 1
;; 2
;; 3
;; 4
;; 5
;; ;Value: 5

;; 1 ]=> (stream-ref x 7)
;; 6
;; 7
;; ;Value: 7

; 3.52
; ========================================================================
;; 1 ]=> (define sum 0)
;; ;Value: sum

;; 1 ]=> (define (accum x)
;;   (set! sum (+ x sum))
;;   sum)
;; ;Value: accum

;; 1 ]=> (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;; ;Value: seq

;; 1 ]=> (define y (stream-filter even? seq))
;; ;Value: y

;; 1 ]=> (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;;                          seq))
;; ;Value: z

;; 1 ]=> (stream-ref y 7)
;; ;Value: 136

;; 1 ]=> (display-stream z)
;; 10
;; 15
;; 45
;; 55
;; 105
;; 120
;; 190
;; 210
;; ;Unspecified return value

;; If our implementation of (delay) wasn't memo-ized, (stream-ref y 7) would still
;; produce the same result but (display-stream z) wouldn't.  Since the variable
;; sum is getting set as a side effect of mapping over the stream, iterating over z
;; after iterating over y means you'd be essentially starting at the 8th element
;; in y's sequence.
;; In the memo-ized version, each element of the stream is only getting set once
;; so you get the same result every time you map over it.

; 3.53
; ========================================================================
;; The stream's elements look like
;; 0: 1
;; 1: (1 + 1, (delay add-streams s s)) = 2
;; 2: (1 + 1) + (1 + 1), (delay add-streams s s) = 4
;; 3: (1 + 1 + 1 + 1) + (1 + 1 + 1 + 1), (delay add-streams s s) = 8
;; Each element is double the previous element.

; 3.54
; ========================================================================
(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorial (cons-stream 1 (mul-streams integers factorial)))

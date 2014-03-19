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

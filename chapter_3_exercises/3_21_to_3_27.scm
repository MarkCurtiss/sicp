; 3.21
; ========================================================================
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;; (define q1 (make-queue))
;;       +-+-+   +-+
;; q1+-->|+|+--->|/|
;;       +|+-+   +-+
;;        |
;;        |
;;        v
;;       +-+
;;       |/|
;;       +-+

;; (insert-queue! q1 'a)
;;       +-+-+
;; q1+-->|+|+|
;;       +|+|+
;;        | |
;;        |-+
;;        v
;;       +-+-+
;;       |+|/|
;;       +|+-+
;;        |
;;        v
;;       +-+
;;       |a|
;;       +-+

;; (insert-queue! q1 'b)
;;       +-+-+
;; q1+-->|+|+|
;;       +|+|+
;;        | |
;;        | +-----+
;;        v       v
;;       +-+-+   +-+-+
;;       |+|+--->|+|/|
;;       +|+-+   +|+-+
;;        |       |
;;        v       v
;;       +-+     +-+
;;       |a|     |b|
;;       +-+     +-+

;; (delete-queue! q1)
;;       +-+-+
;; q1+-->|+|+|
;;       +|+|+
;;        | +-----+
;;        +-------|
;;                v
;;       +-+-+   +-+-+
;;       |+|+--->|+|/|
;;       +|+-+   +|+-+
;;        |       |
;;        v       v
;;       +-+     +-+
;;       |a|     |b|
;;       +-+     +-+

;; (delete-queue! q1)
;;       +-+-+
;; q1+-->|+|+|
;;       +|+|+
;;        | +-----+
;;        +-------|-+
;;                v v
;;       +-+-+   +-+-+
;;       |+|+--->|+|/|
;;       +|+-+   +|+-+
;;        |       |
;;        v       v
;;       +-+     +-+
;;       |a|     |b|
;;       +-+     +-+
;; Note that the front-ptr is pointing to the tail of the entire list
;; but the rear-ptr is still pointing to 'b.  This is why the interpreter
;; prints (() b).

(define (print-queue queue) (front-ptr queue))

; 3.22
; ========================================================================
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (set-front-ptr! item)
      (set! front-ptr item))

    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (empty?)
      (null? front-ptr))

    (define (front-queue)
      (if (empty?)
	  (error "FRONT called with an empty queue")
	   front-ptr))

    (define (insert! item)
      (let ((new-pair (cons item '())))
	(cond ((empty?)
	       (set-front-ptr! new-pair)
	       (set-rear-ptr! new-pair))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set-rear-ptr! new-pair)))))

    (define (delete!)
      (if (empty?)
	  (error "DELETE! called with an empty queue")
	  (set-front-ptr! (cdr front-ptr))))

    (define (print)
      front-ptr)

    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
	    ((eq? m 'rear-ptr) rear-ptr)
	    ((eq? m 'set-front-ptr!) set-front-ptr!)
	    ((eq? m 'set-rear-ptr!) set-rear-ptr!)
	    ((eq? m 'empty?) empty?)
	    ((eq? m 'front) front-queue)
	    ((eq? m 'insert!) insert!)
	    ((eq? m 'delete!) delete!)
	    ((eq? m 'print) print)
	    (else (error "INVALID MESSAGE " m))))
      dispatch))

(define (front-ptr queue)
  (queue 'front-ptr))
(define (rear-ptr queue)
  (queue 'rear-ptr))
(define (set-front-ptr! queue item)
  ((queue 'set-front-ptr!) item))
(define (set-rear-ptr! queue item)
  ((queue 'set-rear-ptr!) item))
(define (empty-queue? queue)
  ((queue 'empty?)))
(define (front-queue queue)
  ((queue 'front)))
(define (insert-queue! queue item)
  ((queue 'insert!) item))
(define (delete-queue! queue)
  ((queue 'delete!)))
(define (print-queue queue)
  ((queue 'print)))
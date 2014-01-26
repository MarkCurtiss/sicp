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

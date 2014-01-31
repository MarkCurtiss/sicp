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

; 3.23
; ========================================================================

(define (make-node item)
  (cons '() (cons item '())))

(define (get-next-ptr-node node)
  (cddr node))

(define (get-prev-ptr-node node)
  (car node))

(define (set-next-ptr-node! node item)
  (set-cdr! (cdr node) item))

(define (set-prev-ptr-node! node item)
  (set-car! node item))

(define (get-value-node node)
  (cadr node))


(define (make-dequeue)
  (cons '() '())
  )

(define (empty-dequeue? dequeue)
  (null? (front-dequeue dequeue)))

(define (front-dequeue dequeue)
  (car dequeue))

(define (rear-dequeue dequeue)
  (cdr dequeue))

(define (set-start-ptr! dequeue item)
  (set-car! dequeue item))

(define (set-end-ptr! dequeue item)
  (set-cdr! dequeue item))

(define (rear-insert-dequeue! dequeue item)
  (let ((new-node (make-node item)))
    (cond ((empty-dequeue? dequeue)
	   (set-start-ptr! dequeue new-node)
	   (set-end-ptr! dequeue new-node))
	  (else
	   (set-next-ptr-node! (rear-dequeue dequeue) new-node)
	   (set-prev-ptr-node! new-node (rear-dequeue dequeue))
	   (set-end-ptr! dequeue new-node))))
  )

(define (front-insert-dequeue! dequeue item)
  (let ((new-node (make-node item)))
    (cond ((empty-dequeue? dequeue)
	   (set-start-ptr! dequeue new-node)
	   (set-end-ptr! dequeue new-node))
	  (else
	   (set-next-ptr-node! new-node (front-dequeue dequeue))
	   (set-prev-ptr-node! (front-dequeue dequeue) new-node)
	   (set-start-ptr! dequeue new-node)))))

(define (front-delete-deque! dequeue)
  (if (empty-dequeue? dequeue)
      (error "front-delete-deque! called on an empty dequeue")
      (set-start-ptr!
       dequeue
       (get-next-ptr-node (front-dequeue dequeue)))))

(define (rear-delete-dequeue! dequeue)
  (if (empty-dequeue? dequeue)
      (error "rear-delete-deque! called on an empty dequeue")
      (begin
	(set-end-ptr! dequeue (get-prev-ptr-node (rear-dequeue dequeue)))
	(set-next-ptr-node! (rear-dequeue dequeue) '()))))

(define (print-dequeue dequeue)
  (define (iter node results)
    (if (null? node)
	results
	(iter
	 (get-next-ptr-node node)
	 (append results (list (get-value-node node))))))

  (iter (front-dequeue dequeue) '()))

; 3.24
; ========================================================================
(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
	  ((same-key? key (caar records)) (car records))
	  (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
	(if record
	    (cdr record)
	    false)))

    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
	(if record
	    (set-cdr! record value)
	    (set-cdr! local-table
		      (cons (cons key value) (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; 3.25
; ========================================================================
(define (make-multi-key-table)
  (define (assoc keys records)
    (cond ((null? records) false)
	  ((equal? keys (caar records)) (car records))
	  (else (assoc keys (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (let ((record (assoc keys (cdr local-table))))
	(if record
	    (cdr record)
	    false)))

    (define (insert! keys value)
      (let ((record (assoc keys (cdr local-table))))
	(if record
	    (set-cdr! record value)
	    (set-cdr! local-table
		      (cons (cons keys value) (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; 3.26
; ========================================================================
;; The head pointer will point to the root node of the binary tree.  (insert!)
;; will find the appropriate place for the key using the same traversal
;; algorithm as in 2.66 (if the node contains your key, return that. if it is
;; greater than your key, traverse the left subtree.  otherwise, traverse the
;; right subtree.  (lookup) will use the same algorithm.

;; This reduces the complexity of insertion and retrieval from O(n) to
;; O(log(n)).

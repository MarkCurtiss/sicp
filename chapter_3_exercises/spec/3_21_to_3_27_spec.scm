(load "3_21_to_3_27.scm")

(describe "Queues"
  (it "prints a queue"
    (lambda ()
      (define a-queue (make-queue))

      (for-each
       (lambda (x) (insert-queue! a-queue x))
       (list 'a 'b 2 4 '(6 8)))

      (delete-queue! a-queue)

      (assert (equal?
	       (print-queue a-queue)
	       '(b 2 4 (6 8))))))
)

(describe "Node"
  (it "has a value and links to the previous and next node"
    (lambda ()
      (define first-node (make-node 'a))
      (define second-node (make-node 'b))

      (set-next-ptr-node! first-node second-node)
      (set-prev-ptr-node! second-node first-node)

      (assert (equal?
	       (get-value-node first-node)
	       'a))

      (assert (equal?
	       (get-value-node second-node)
	       'b))

      (assert (equal?
	       (get-next-ptr-node first-node)
	       second-node))

      (assert (equal?
	       (get-next-ptr-node second-node)
	       '()))

      (assert (equal?
	       (get-prev-ptr-node second-node)
	       first-node))

      (assert (equal?
	       (get-value-node (get-prev-ptr-node second-node))
	       (get-value-node first-node)))

      (assert (equal?
	       (get-prev-ptr-node first-node)
	       '()))))

)

(describe "Dequeue"
  (it "tells you if you have an empty dequeue or not"
    (lambda ()
      (define dq (make-dequeue))

      (assert (empty-dequeue? dq))))

  (it "maintains pointers to the start and end of the dequeue"
    (lambda ()
      (define dq (make-dequeue))

      (rear-insert-dequeue! dq 'a)
      (rear-insert-dequeue! dq 'b)
      (rear-insert-dequeue! dq 3)

      (assert (equal?
	       (get-value-node (front-dequeue dq))
	       'a))

      (assert (equal?
	       (get-value-node (rear-dequeue dq))
	       3))))

  (it "lets you insert at the start of the dequeue"
    (lambda ()
      (define dq (make-dequeue))

      (front-insert-dequeue! dq 'a)
      (front-insert-dequeue! dq 'b)
      (front-insert-dequeue! dq 3)

      (assert (equal?
	       (print-dequeue dq)
	       '(3 b a)))))

  (it "lets you insert at the end of the dequeue"
    (lambda ()
      (define dq (make-dequeue))

      (rear-insert-dequeue! dq 'a)
      (rear-insert-dequeue! dq 'b)
      (rear-insert-dequeue! dq 3)

      (assert (equal?
	       (print-dequeue dq)
	       '(a b 3)))))

  (it "lets you delete from the start of the dequeue"
    (lambda ()
      (define dq (make-dequeue))

      (for-each (lambda (x) (rear-insert-dequeue! dq x)) '(a b 3 4))

      (front-delete-deque! dq)
      (front-delete-deque! dq)

      (assert (equal?
	       (print-dequeue dq)
	       '(3 4)))))

  (it "lets you delete from the rear of the dequeue"
    (lambda ()
      (define dq (make-dequeue))

      (for-each (lambda (x) (rear-insert-dequeue! dq x)) '(a b 3 4))

      (rear-delete-dequeue! dq)
      (rear-delete-dequeue! dq)

      (assert (equal?
	       (print-dequeue dq)
	       '(a b)))))

)

(describe "Tables"
  (it "lets you define your own equality test"
    (lambda ()
      (define (double-key? key-1 key-2) (= (* 2 key-1) key-2))

      (define table (make-table double-key?))

      ((table 'insert!) 4 9)

      (assert (equal?
	       ((table 'lookup) 2)
	       9))

      (assert (equal?
	       ((table 'lookup) 4)
	       false))))
)

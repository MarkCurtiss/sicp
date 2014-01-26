(load "3_21_to_3_27.scm")

(describe "Queues"
  (it "prints a queue"
    (lambda ()
      (define a-queue (make-queue))

      (for-each
       (lambda (x) (insert-queue! a-queue x))
       (list 'a 'b 2 4 '(6 8)))

      (assert (equal?
	       (print-queue a-queue)
	       '(a b 2 4 (6 8))))))
)

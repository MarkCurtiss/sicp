(load "3_12_to_3_20.scm")

(describe "Counting pairs in a list"
  (it "counts the number of pairs in a list"
    (lambda ()
      (define x (list 'a 'b 'c))
      (assert (=
	       (count-pairs x)
	       3))))

  (it "correctly counts distinct pairs in a list with multiple pointers to the same pair"
    (lambda ()
      (define x (list 'a 'b 'c))
      (set-car! x (cddr x))
      (assert (=
	       (count-pairs x)
	       3))))
)

(describe "Detecting cycles in a list"
  (it "returns false if the list has no cycles"
    (lambda ()
      (define x (list 'a 'b 'c))

      (assert (false? (is-cyclical? x)))))

  (it "returns true if the list has cycles"
    (lambda ()
      (define x (list 'a 'b 'c))
      (make-cycle x)

      (assert (is-cyclical? x))))
)

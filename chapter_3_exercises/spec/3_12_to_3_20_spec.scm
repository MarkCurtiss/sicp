(load "3_12_to_3_20.scm")

(describe "Counting pairs in a list"
  (it "counts the number of pairs in a list"
    (lambda ()
      (define x (list 'a 'b 'c))
      (assert (= 3 (count-pairs x)))))

  (it "correctly counts distinct pairs in a list with multiple pointers to the same pair"
    (lambda ()
      (define x (list 'a 'b 'c))
      (set-car! x (cddr x))
      (assert (= 3 (count-pairs x)))))

)
; 5.45
; ========================================================================
(load "book_code/load-eceval-compiler.scm")

;; 1 ]=>
;; (compile-and-go
;;  '(define (factorial n)
;;     (if (= n 1)
;;         1
;;         (* (factorial (- n 1)) n))))

;; (total-pushes = 0 maximum-depth = 0)
;;  EC-Eval value:
;; ok

;;  EC-Eval input:
;; (factorial 1)

;; (total-pushes = 7 maximum-depth = 3)
;;  EC-Eval value:
;; 1

;;  EC-Eval input:
;; (factorial 2)

;; (total-pushes = 13 maximum-depth = 5)
;;  EC-Eval value:
;; 2

;;  EC-Eval input:
;; (factorial 3)

;; (total-pushes = 19 maximum-depth = 8)
;;  EC-Eval value:
;; 6

;; EC-Eval input:
;; (factorial 4)

;; (total-pushes = 25 maximum-depth = 11)
;; EC-Eval value:
;; 24

;; a.
;; Compiled version
;; Num pushes: 6n + 1
;; Max depth: 3n - 1 (for n > 2)

;; Interpreted version
;; Num pushes: 16 * (2n - 1)
;; Max depth: 8 + 5(n - 1)

;; Special-purpose factorial machine version
;; Num pushes: 2*n - 2
;; Max depth: 2*n - 2

;; Compiled vs interpreted
;; For the compiled (factorial 1000)
;; (total-pushes = 6001 maximum-depth = 2999)
;; For the interpreted (factorial 1000)
;; (total-pushes = 31984 maximum-depth = 5003)
;; The total pushes for the compiled version appear to be 1/5th that
;; of the interpreted version.
;; The max depth for the compiled version appear to be 3/5th that
;; of the interpreted version.

;; Special-purpose vs interpreted
;; For the special-purpose (factorial 1000)
;; (total-pushes = 1998 maximum-depth = 1998)
;; For the interpreted (factorial 1000)
;; (total-pushes = 31984 maximum-depth = 5003)
;; The total pushes for the special-purpose machine appear to be 1/16th that
;; of the interpreted version. (!)
;; The max depth for the special-purpose machine appear to be 2/5th that
;; of the interpreted version.

;; The special-purpose version is much more efficient than the compiled version
;; and dramatically embarrasingly more efficient than the interpreted version.

;; b.

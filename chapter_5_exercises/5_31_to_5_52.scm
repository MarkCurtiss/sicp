; 5.31
; ========================================================================
;; 1. saves and restores the env register around the evaluation of the operator
;; 2. saves and restores env around the evaluation of each operand (except the final one)
;; 3. saves and restores argl around the evaluation of each operand
;; 4. saves and restores proc around the evaluation of the operand sequence

(f 'x 'y)
;; You could optimize out 1, 2, 3, 4

((f) 'x 'y)
;; You could optimize out 1, 2, 3, 4

(f (g 'x) y)
;; You could optimize out 1, 2

(f (g 'x) 'y)
;; You could optimize out 1, 2

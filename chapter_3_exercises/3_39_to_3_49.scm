; 3.39
; ========================================================================
;; 101:P1 sets x to 100 and then P2 increments x to 101.
;; 121:P2 increments x to 11 and then P1 sets x to x times x.
;; 100:P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.

; 3.40
; ========================================================================
;; 10 * 10 == 100, 100 * 100 * 100 = 1,000,000
;; 10 * (100*100*100) ==  10,000
;; 10 * 10 * 10 = 1,000
;; 10 * 10 = 100
;; 10 * 10 * (100 * 100) = 100,000

;; If we serialize the operations, the only possibilities are:
;; 10 * 10 == 100, 100 * 100 * 100 = 1,000,000


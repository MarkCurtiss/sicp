; 3.38
; ========================================================================
;; a.
;; $45, $35, $40, $50

;; b.
;; The following sequence of interleaved events incorrectly leaves the
;; bank with $40 when it should have $45.

"
+-----+        +----+           +----+
|Peter|        |Paul|           |Mary|
+--+--+        +--+-+           +-+--+
   |              |               |
   |              |               |
   v              v               |
Access: $100   Access: $100       |
   +              +               |
   v              |               |
Balance += $10    |               |
   +              v               |
   |           Balance -= $20     |
   |               +              |
   |           Balance: $80       v
   v                          Access: $80
Balance = $110                    +
                                  |
                                  v
                              Balance /= 2
                                  +
                                  |
                                  |
                                  v
                              Balance: $40
"
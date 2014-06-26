; 4.25
; ========================================================================
; When you try and evaluate (factorial 5) you'll recurse indefinitely as
; the second argument to (unless) is another call to (factorial).
; This would work with a lazily evaluated language, however.

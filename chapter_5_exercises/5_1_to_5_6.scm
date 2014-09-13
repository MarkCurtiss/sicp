; 5.1
; ========================================================================
;; Here is the data flow diagram.  Note that ASCII art prevents me from
;; doing trapezoids or circles with x's in them:
"
               +--+-----------------------------+
-+-+      +----+--+                             |   +-------+
|> +------+counter+---+                         |   |product<+
---+      +-------+   |                         |   +--+----++
 |                |   |                         |      |     |
 |                |   +-----+                   |      |     |
+++               |         |                   |      |     |
|n|      +-+    +-v---+     |                  +++     |     |
+-+      |1+----+ plus|     |counter<-cp1      |*+-----+     |
         +-+    +-+---+     |                  +++           |
                  |         |                   |            |product<-pc
                  |         |                   |            |
                +-v-+---++--+                 +-v+           |
                |cp1|cp1<-r                   |pc|pc<-r      |
                +---+                         ++-+           |
                                               |             |
                                               |             |
                                               +-------------+
"

;; Here is the control diagram.
"
     start
       +
       |
       |
       |
      +--+  yes
+---> |> +-------->done
|     +--+
|      |
|      |
|      |
|    +-v++--+
|    |pc<-r |
|    |      |
|    +-+----+
|      |
|    +-v-++-+
|    |cp1<-r|
|    |      |
|    +-+----+
|      |
|    +-v-----++--+
|    |product<-pc|
|    |           |
|    +-+---------+
|      |
|    +-v-----++---+
|    |counter<-cp1|
|    |            |
|    +---+--------+
|        |
|        |
|        |
+--------+
"

; 5.2
; ========================================================================
(controller
 (assign product 1)
 (assign counter 1)

 test-counter
   (test (op >) (reg counter) (reg n))
   (branch (label factorial-done))
   (assign pc (op *) (reg product) (reg counter))
   (assign cp1 (op +) (reg counter) (constant 1))
   (assign product (reg pc))
   (assign counter (reg cp1))
   (goto (label test-counter))
 factorial-done)

; 5.3
; ========================================================================
;; Assuming primitive operations.
(controller
 (assign guess 1.0)

 test-guess
   (test (op good-enough?) (reg guess))
   (branch (label sqrt-done))
   (assign guess (op improve) (reg guess))
   (goto (label test-guess))
 sqrt-done)

;; Expanding the primitive operations.
(controller
 (assign guess 1.0)
 (assign x (op read))

 test-guess
;; good-enough?
   (assign sqrguess (op square) (reg guess))
   (assign sqrminusx (op -) (reg sqrguess) (reg x))
   (assign absresult (op abs) (reg sqrminusx))
   (test (op <) (reg absresult) (constant 0.001))
   (branch (label sqrt-done))
;; improve
   (assign xdivguess (op /) (reg x) (reg guess))
   (assign avgguess (op avg) (reg xdivguess) (reg guess))
   (assign guess (reg avgguess))
   (goto (label test-guess))
 sqrt-done)

;; I drew diagrams for these but they are drawn in a pen on a notebook
;; in front of me and I don't feel like drawing them in ASCII art.
;; I guess I can show my study-group colleagues the drawing when we
;; meet up.

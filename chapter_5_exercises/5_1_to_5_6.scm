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
 (assign pc 1)
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

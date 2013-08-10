; 2.44
; ========================================================================
#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;;(paint (right-split einstein 4))
;;.


; 2.45
; ========================================================================
(define (split larger-location smaller-location)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split larger-location smaller-location) painter (- n 1))))
          (larger-location painter (smaller-location smaller smaller)))
     ))
 )

(define right-split-proc (split beside below))
(define up-split-proc (split below beside))

;> (paint (right-split-proc einstein 1))
;.
;> (paint (up-split-proc einstein 1))
;.

; 2.46
; ========================================================================
(define (make-vector x y)
  (cons x y))

(define (xcor-vect vector) (car vector))
(define (ycor-vect vector) (cdr vector))

(define test-vector1 (make-vector 1 2))
(define test-vector2 (make-vector 6 8))

(define (add-vect vector1 vector2)
  (make-vector
   (+ (xcor-vect vector1) (xcor-vect vector2))
   (+ (ycor-vect vector1) (ycor-vect vector2))))

;> (add-vect test-vector1 test-vector2)
;'(7 . 10)

(define (sub-vect vector1 vector2)
  (make-vector
   (- (xcor-vect vector1) (xcor-vect vector2))
   (- (ycor-vect vector1) (ycor-vect vector2))))

;> (sub-vect test-vector1 test-vector2)
;'(-5 . -6)

(define (scale-vect vector scalar)
  (make-vector
   (* (xcor-vect vector) scalar)
   (* (ycor-vect vector) scalar)))

;> (scale-vect test-vector2 8)
;'(48 . 64)

; 2.47
; ========================================================================
(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame1 frame)
   (car frame))

(define (edge1-frame1 frame)
   (car (cdr frame)))

(define (edge2-frame1 frame)
   (car (cdr (cdr frame))))

;> (origin-frame1 (make-frame-1 (make-vector 0 0) test-vector1 test-vector2))
;'(0 . 0)
;> (edge1-frame1 (make-frame-1 (make-vector 0 0) test-vector1 test-vector2))
;'(1 . 2)
;> (edge2-frame1 (make-frame-1 (make-vector 0 0) test-vector1 test-vector2))
;'(6 . 8)

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-2 frame)
   (car frame))

(define (edge1-frame-2 frame)
   (car (cdr frame)))

(define (edge2-frame-2 frame)
     (cdr (cdr frame)))

;> (origin-frame-2 (make-frame-2 (make-vector 0 0) test-vector1 test-vector2))
;'(0 . 0)
;> (edge1-frame-2 (make-frame-2 (make-vector 0 0) test-vector1 test-vector2))
;'(1 . 2)
;> (edge2-frame-2 (make-frame-2 (make-vector 0 0) test-vector1 test-vector2)))
;'(6 . 8)

; 2.48
; ========================================================================
(define (make-segmentt v1 v2)
  (cons v1 v2))

(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

; 2.49
; ========================================================================
; a.
(define origin (make-vector 0 0))
(define upper-left (make-vector 0 0.9))
(define upper-right (make-vector 0.9 0.9))
(define lower-right (make-vector 0.9 0))

(define frame-drawer
  (segments->painter
   (list (make-segment origin upper-left)
         (make-segment upper-left upper-right)
         (make-segment upper-right lower-right)
         (make-segment lower-right origin))))
;> (paint frame-drawer)
;.

;b.
(define x-drawer
  (segments->painter
   (list (make-segment upper-left lower-right)
         (make-segment upper-right origin))))

;> (paint x-drawer)
;.

;c.
(define (midpoint vector1 vector2)
  (scale-vect (add-vect vector1 vector2) .5))

(define diamond-drawer
  (segments->painter
   (list (make-segment (midpoint origin lower-right) (midpoint origin upper-left))
         (make-segment (midpoint origin upper-left) (midpoint upper-left upper-right))
         (make-segment (midpoint upper-left upper-right) (midpoint upper-right lower-right))
         (make-segment (midpoint upper-right lower-right) (midpoint lower-right origin) ))))

;> (paint diamond-drawer)
;.

;d.
;Congratulations SICP, you finally found a problem so annoying that I didn't even bother!
;I plagiarized this solution from http://www.billthelizard.com/2011/10/sicp-249-defining-primitive-painters.html
(define wave-drawer
  (segments->painter
    (list
       (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144)))))

;> (paint wave-drawer)
;.

; 2.50
; ========================================================================
(define (flip-horizontal painter)
  ((transform-painter
   (make-vector 1 0)
   (make-vector 0.0 0)
   (make-vector 1 1)) painter))

;(paint (flip-horizontal wave-drawer))
;.

;> (paint (flip-horizontal einstein))
;.

(define (rotate-180 painter)
  ((transform-painter
    (make-vector 1 1)
    (make-vector 0 1)
    (make-vector 1 0)) painter))

;> (paint (rotate-180 einstein))
;.

(define (rotate-270 painter)
  ((transform-painter
    (make-vector 0 1)
    (make-vector 0 0)
    (make-vector 1 1)) painter))

;> (paint (rotate-270 einstein))
;.

; 2.51
; ========================================================================
(define (below1 painter1 painter2)
    (let ((split-point (make-vect 0.0 0.5)))
      (let ((paint-top
             ((transform-painter split-point
                                 (make-vect 1.0 0.5)
                                 (make-vect 0.0 1.0)) painter2))
            (paint-bottom
             ((transform-painter origin
                                 (make-vect 1.0 0.0)
                                 split-point) painter1)))
            (lambda (frame)
              (paint-top frame)
              (paint-bottom frame)))))

;> (paint (below1 wave-drawer einstein))
;.

(define (rotate-90 painter)
   ((transform-painter
    (make-vector 1 0)
    (make-vector 1 1)
    (make-vector 0 0)) painter))

(define (below2 painter1 painter2)
  (rotate-90 (beside (rotate-270 painter1) (rotate-270 painter2))))
;
;> (paint (below2 wave-drawer einstein))
;.

; 2.52
; ========================================================================
;again blatantly plagiarized
(define wave-smile-drawer
  (segments->painter
    (list
       (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))
  (make-segment
   (make-vect 0.395 0.916)
   (make-vect 0.410 0.916))
  (make-segment
   (make-vect 0.376 0.746)
   (make-vect 0.460 0.790))
  )))

(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (* n 2)))
            (right (right-split painter (* n 2))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (* n 2))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;> (paint (corner-split einstein 2))
;.
;> (paint (corner-split2 einstein 2))
;.

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (my-square-limit painter n)
  (let ((combine4 (square-of-four identity flip-vert rotate270 flip-vert)))
    (combine4 (corner-split painter n))))

;> (paint (square-limit einstein 2))
;.
;> (paint (my-square-limit einstein 2))
;.
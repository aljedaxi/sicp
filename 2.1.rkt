#lang sicp

; 2.1, pp 87
(define (make-rat n d)
  ; n {number} numerator
  ; n {number} denominator
  (define (negative n)
    (if (negative? n)
        n
        (* -1 n)))
  (define (positive n)
    (if (positive? n)
        n
        (* -1 n)))
  (cond
    [(and (positive? n) (positive? d)) (cons n d)]
    [(and (negative? n) (negative? d)) (cons n d)]
    [else (cons (negative n) (positive d))]))

; 2.2 pp89
; line segments are represented as having a pair of points

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (average x y)
  (/ (+ x y) 2))

(define (mid-point segment)
  (let ([start (start-segment segment)]
        [end (end-segment segment)])
    (make-point (average (x-point start) (x-point end)) (average (y-point start) (y-point end)))))

; 2.3 pp90
;
(define (Rect1 upper-left upper-right bottom-left)
  (cons (make-segment upper-left upper-right) (make-segment upper-left bottom-left)))
(define (vert-segment rect)
  (car rect))
(define (horizontal-segment rect)
  (cdr rect))
(define (difference x y)
  (if (> x y)
      (- x y)
      (- y x)))
(define (height rect)
  (define vert (vert-segment rect))
  (let ([start (start-segment vert)]
        [end (end-segment vert)])
    (difference (y-point start) (y-point end))))
(define (width rect)
  (define hor (horizontal-segment rect))
  (let ([start (start-segment hor)]
        [end (end-segment hor)])
    (difference (x-point start) (x-point end))))

(define my-epic-rectangle-1 (Rect1 (make-point 4 4) (make-point 4 8) (make-point 2 4)))

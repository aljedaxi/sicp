#lang sicp

; run `:repl` to get ur repl going. hit f5 to reload that shit.
; use ctrl-alt-y to insert a λ
; to make a new file, try ctrl-x ctrl-d

(define (inc n) (+ n 1))
(define (cube n) (* n n n))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (iterative-sum term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; (accumulate combiner null-term a next b)
(define (accumulate combiner null-term term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (combiner result (term a)))))
    (iter a null-term))

(define (recursive-acc combiner null-term term a next b)
  (if (> a b)
      null-term
      (combiner (term a)
                (recursive-acc combiner null-term term (next a) next b))))

(define (acc-sum term a next b)
  (accumulate + 0 term a next b))

; (define (f g) (g 2))
; (f f) => (f 2) => (2 2)
; eventually it takes 2 to also be a function lol
(define (average n m)
  (/ (+ n m) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (double f) (lambda (x) (f (f x))))
;(((double (double double)) inc) 5)
(define (square n) (* n n))
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f n)
  (if (= 1 n)
      f
      (compose f (repeated f (- n 1)))))
; (repeated square 2)

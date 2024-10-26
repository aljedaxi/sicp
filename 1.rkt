#lang sicp

; run `:repl` to get ur repl going. hit f5 to reload that shit
; use ctrl-alt-y to insert a λ
; to make a new file, try ctrl-x ctrl-d

(define (pred n) (- n 1))

(define (double n) (* 2 n))
(define (halve n) (/ n 2))

(define (fast-mult n m)
  (cond ((= 1 m) n)
        ((even? m) (double (fast-mult n (halve m))))
        (else      (+ n (fast-mult n (pred m))))))

(define (peasant n m)
  (define (main remainders n m)
    (cond ((= m 1) (+ remainders n))
          ((even? m) (main remainders (double n) (halve m)))
          (else (main (+ remainders n) n (pred m)))))
  (main 0 n m))

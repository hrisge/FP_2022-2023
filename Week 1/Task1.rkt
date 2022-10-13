#lang racket

(define (my-gcd x y)
  (cond
    [(zero? x) y]
    [(zero? y) x]
    [else (my-gcd y (remainder x y))]
    )
  )
    



(= (my-gcd 5 13) 1)
(= (my-gcd 13 1235) 13)
(= (my-gcd 13 5) 1)

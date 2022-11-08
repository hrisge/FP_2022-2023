#lang racket

(define (upper-bound f y)
  (λ (x)
    (if (> y (f x))
        y
        (f x)))
  
  )


(= ((upper-bound (λ (x) (* 2 x)) 100) 50) 100)
(= ((upper-bound (λ (x) (* 2 x)) 100.236) 500.002) 1000.004)
(= ((upper-bound (λ (x) (* 2 x)) 80) 3) 80)
(= ((upper-bound (λ (x) (* 2 x)) 70) 30) 70)
(= ((upper-bound (λ (x) (* 2 x)) 30) 70) 140)
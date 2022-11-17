#lang racket

(define (kth-max-min xs)
  (λ (x) (if (< (length (remove-duplicates (filter negative? xs))) x)
             (error "No such number")
             (list-ref (sort (remove-duplicates (filter negative? xs)) >) (sub1 x))
             )))


(= ((kth-max-min '(-1)) 1) -1)
(= ((kth-max-min '(-1 -5 -6 -6 -6 -6)) 2) -5)
(= ((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0)) 2) -2)
((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3) ; error: No such number!
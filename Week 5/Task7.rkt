#lang racket

(define (kth-max-min xs)

  (define (get-ith-el xs i)
    (cond
      [(empty? xs) (error "No such number")]
      [(= 1 i) (car xs)]
      [else (get-ith-el (cdr xs) (sub1 i))]))

  (λ (x) (get-ith-el (sort (remove-duplicates (filter negative? xs)) >) x)) )


(= ((kth-max-min '(-1)) 1) -1)
(= ((kth-max-min '(-1 -5 -6 -6 -6 -6)) 2) -5)
(= ((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0)) 2) -2)
;((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3) ; error: No such number!
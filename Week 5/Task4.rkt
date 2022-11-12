#lang racket

(define (rev-fold xs)
  (foldr (λ (x acc) (+ (* 10 acc) x)) 0 xs))

(define (rev-lin-iter xs)
  (define (helper result leftovers)
    (if (empty? leftovers)
        result
        (helper (+ (* 10 result) (car leftovers)) (cdr leftovers))))

  (helper 0 (reverse xs))
  )

; using folding
(= (rev-fold '(1 2 3)) 321)
(= (rev-fold '(1 2 3 4 5 6 7 8 9)) 987654321)

; using a linearly iterative procedure
(= (rev-lin-iter '(1 2 3)) 321)
(= (rev-lin-iter '(1 2 3 4 5 6 7 8 9)) 987654321)
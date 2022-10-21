#lang racket

(define (find-sum a b n)
  (define (helper counter n-buff)
      (cond
        [(= n-buff 1) (+ a b)]
        [else (+ (* (expt 2 n) b counter) (helper (add1 counter) (sub1 n-buff)))]
        )
    )

  (cond
    [(or (negative? a) (negative? b) (< n 4)) (error "a and n must be possitive and b must be more than 3")]
    [else (helper 1 n)]
    )

  )



(= (find-sum 0 2 10) 3578) ; 510 + 1022 + 2046
(= (find-sum 5 3 5) 174) ; 26 + 50 + 98
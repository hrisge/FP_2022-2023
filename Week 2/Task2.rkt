#lang racket

(define (sum-digits-iter n)
  (define (helper n result)
    (if (zero? n)
        result
        (helper (quotient n 10) (+ result (remainder n 10)))
        )
    )

  (if (negative? n)
      (error "n was negative")
      (helper n 0)
      )
  )



(= (sum-digits-iter 12345) 15)
(= (sum-digits-iter 123) 6)
(sum-digits-iter -13)
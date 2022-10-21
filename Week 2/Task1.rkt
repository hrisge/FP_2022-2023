#lang racket

(define (count-digits-rec n)
  (cond
    [(negative? n) (error "n was negative")]
    [(zero? n) 0]
    [else (add1 (count-digits-rec (quotient n 10)))]
    )
  )

(define (count-digits-iter n)
  (define (helper n counter)
    (if(zero? n)
    counter
    (helper (quotient n 10) (add1 counter))
    )
    )

  (if (negative? n)
      (error "n was negative")
      (helper n 0)
      )
)


(= (count-digits-iter 12345) 5)
(= (count-digits-iter 123) 3)

(= (count-digits-rec 12345) 5)
(= (count-digits-rec 123) 3)
(count-digits-iter -13)
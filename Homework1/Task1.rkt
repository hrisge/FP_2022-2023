#lang racket

(require math/number-theory)

(define (sum-digits number result)
  (if (zero? number)
      result
      (sum-digits (quotient number 10) (+ result (remainder number 10)))
      )  
  )


(define (count-specials k a b)
  (cond
    [(> a b) 0]
    [(and (divides? k a) (divides? k (sum-digits a 0))) (add1 (count-specials k (add1 a) b))]
    [else (count-specials k (add1 a) b)]
  )
  )

(count-specials 3 3 9)
(count-specials 5 10 100)
(count-specials 8 100 200)
(count-specials 15 1000 2000)
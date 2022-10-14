#lang racket

(define (rev n)
  (define (helper n curr-res)
   (if (= n 0)
     curr-res
     (helper (quotient n 10) (+ (* curr-res 10) (remainder n 10)))
  )
    )
  (if (negative? n)
    (error "The number is negative")
    (helper n 0)
    )
  )


(= (rev 1) 1)
(= (rev 123) 321)
(= (rev 987654321) 123456789)
#lang racket

(define (interesting? n)
  (define (sum-of-digits cut-number result)
    (if (not (zero? cut-number))
        (sum-of-digits (quotient cut-number 10) (+ result (remainder cut-number 10)))
        result
      )
    )
  (cond
    [(negative? n) (error "n must be possitive")]
    [(zero? (remainder n (sum-of-digits n 0))) #t]
    [else #f]
      )
  )



(equal? (interesting? 410) #t)
(equal? (interesting? 212) #f)
(equal? (interesting? 567) #f)
(equal? (interesting? 70) #t)
(equal? (interesting? 5) #t)
(equal? (interesting? 4) #t)
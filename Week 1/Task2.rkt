#lang racket

(define (leap-year-one-line?  year)
  (or (and (zero? (remainder year 4)) (not (zero? (remainder year 100)))) (zero? (remainder year 400))
  )
  )

(define (is-leap-year-guards? year)
  (cond
    [(zero? (remainder year 400)) #t]
    [(zero? (remainder year 100)) #f]
    [(zero? (remainder year 4)) #t]
    [else #f]
    )
  )

(equal? (leap-year-one-line? 2020) #t)
(equal? (leap-year-one-line? 1988) #t)
(equal? (leap-year-one-line? 1600) #t)
(equal? (leap-year-one-line? 2400) #t)
(equal? (leap-year-one-line? 2023) #f)
(equal? (leap-year-one-line? 1700) #f)
(equal? (leap-year-one-line? 1800) #f)
(equal? (leap-year-one-line? 2100) #f)

(equal? (is-leap-year-guards? 2020) #t)
(equal? (is-leap-year-guards? 1988) #t)
(equal? (is-leap-year-guards? 1600) #t)
(equal? (is-leap-year-guards? 2400) #t)
(equal? (is-leap-year-guards? 2023) #f)
(equal? (is-leap-year-guards? 1700) #f)
(equal? (is-leap-year-guards? 1800) #f)
(equal? (is-leap-year-guards? 2100) #f)
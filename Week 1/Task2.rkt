#lang racket

(define (leap-year-one-line?  year)
  (or (and (= (remainder year 4) 0) (not (zero? (remainder year 100)))) (= (remainder year 400) 0))
  )

(define (is-leap-year-guards? year)
  (cond
    [(= (remainder year 400) 0) #t]
    [(= (remainder year 100) 0) #f]
    [(= (remainder year 4) 0) #t]
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
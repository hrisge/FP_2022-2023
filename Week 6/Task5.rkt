#lang racket

(require racket/trace)
(define (trailing-zeros n)
  (define (helper result delim)
    (if (> delim n)
        result
        (helper (+ result (quotient n delim)) (* 5 delim))))

  (if (negative? n)
      (error "N is negative!")
      (λ (p?) (p? (helper 0 5))))
  )

(equal? ((trailing-zeros 6) even?) #f)
(equal? ((trailing-zeros 1000) even?) #f)
(equal? ((trailing-zeros 100000) even?) #f)
(equal? ((trailing-zeros 1000000000) even?) #t)
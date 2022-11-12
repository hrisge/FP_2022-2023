#lang racket

(require math/number-theory)

(define (factorize n)
  (define (helper results leftover counter)
    (cond
      [(= 1 leftover) results]
      [(and (prime? counter) (divides? counter leftover)) (helper (append results (list counter)) (quotient leftover counter) counter)]
      [else (helper results leftover (add1 counter))]
      ))
  (if (negative? n)
      (error "N is negative")
      (helper null n 2)))


(equal? (factorize 2) '(2))
(equal? (factorize 6) '(2 3))
(equal? (factorize 13) '(13))
(equal? (factorize 123) '(3 41))
(equal? (factorize 152) '(2 2 2 19))
(equal? (factorize 12356498) '(2 7 11 19 41 103))
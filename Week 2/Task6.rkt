#lang racket

(require math/number-theory)

(define (sum-special-primes n d)
  (define (is-digit-in-number? number digit)
    (cond
      [(zero? number) #f]
      [(= digit (remainder number 10)) #t]
      [else (is-digit-in-number? (quotient number 10) digit)]
    )
    )
  (define (helper counter current-number result)
    (cond
      [(= counter n) result]
      [(and (prime? current-number) (is-digit-in-number? current-number d)) (helper (add1 counter) (add1 current-number) (+ result current-number))]
      [else (helper counter (add1 current-number) result)]
      )
    )

  (cond
    [(or (negative? n) (negative? d)) (error "n and d must be possitive")]
    [(zero? n) 0]
    [else (helper 0 1 0)]
    )
  )


(= (sum-special-primes 5 2) 392)
(= (sum-special-primes 5 3) 107)
(= (sum-special-primes 10 3) 462)
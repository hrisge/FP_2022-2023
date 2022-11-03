#lang racket

(require math/number-theory)
(require racket/trace)

(define (sum-digits-iter n)
  (define (helper n result)
    (if (zero? n)
        result
        (helper (quotient n 10) (+ result (remainder n 10)))))
  (if (negative? n)
      (error "n was negative")
      (helper n 0)))


(define (sum-divisible-numbers start finish k)
  (cond
    [(or (negative? start) (negative? finish) (negative? k)) (error "start finish or k is negative")]
    [(< finish start) (sum-divisible-numbers finish start k)]
    [(= start finish) (if (divides? k (sum-digits-iter start))
                          start
                          0
                          )]
    [(divides? k (sum-digits-iter start)) (+ start (sum-divisible-numbers (add1 start) finish k))]
    [else (sum-divisible-numbers (add1 start) finish k)]
    )
  )


(= (sum-divisible-numbers 0 10 5) 5)
(= (sum-divisible-numbers 0 100 5) 990)
(= (sum-divisible-numbers 100 0 5) 990)
#lang racket

(require math/number-theory)
(require racket/trace)

(define (number-len n)
  (if (zero? n)
      0
      (add1 (number-len (quotient n 10)))
      )
  )

(define (remove-first-occurrence n d)

  (define (helper leftover remainer)
    (cond
      [(zero? leftover) remainer]
      [(= (remainder leftover 10) d) (if (zero? remainer)
                                         (quotient leftover 10)
                                         (+ (* (quotient leftover 10) (expt 10 (number-len remainer))) remainer))]
      [else (helper (quotient leftover 10) (+ (* (remainder leftover 10) (expt 10 (number-len remainer))) remainer))]
      )
    )

  (if (or (negative? n) (negative? d))
      (error "The number or the digit is negative")
      (helper n 0)
      )
  )


(= (remove-first-occurrence 15365 5) 1536)
(= (remove-first-occurrence 15360 0) 1536)
(= (remove-first-occurrence 15300 0) 1530)
(= (remove-first-occurrence 15365 1) 5365)
(= (remove-first-occurrence 35365 3) 3565)
(= (remove-first-occurrence 1212 1) 122)
(= (remove-first-occurrence 1212 2) 121)
(= (remove-first-occurrence (remove-first-occurrence 1212 1) 1) 22)
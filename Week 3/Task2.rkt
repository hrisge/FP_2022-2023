#lang racket

(require math/number-theory)
(require racket/trace)

(define (find-number-of-nulls n)
  (define (helper leftover result)
    (cond
      [(zero? leftover) result]
      [(zero? (remainder leftover 10)) (helper (quotient leftover 10) (add1 result))]
      [else (helper (quotient leftover 10) result)]
      )
    )
  (if (negative? n)
      (error "Number is negative")
      (helper n 0))
  )

(define (find-max n)
  (define (helper max-result leftover)
    (cond
      [(zero? leftover) max-result]
      ;[(= max-result 9) 9]
      [(< max-result (remainder leftover 10)) (helper (remainder leftover 10) (quotient leftover 10))]
      [else (helper max-result (quotient leftover 10))]
      )
    )
  (helper (remainder n 10) (quotient n 10))
  )

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

(define (sort-n n)
  (define (helper leftover result)
    (if (zero? leftover)
        result
        (helper (remove-first-occurrence leftover (find-max leftover)) (+ (* 10 result) (find-max leftover)))
        )
    )

  (if (negative? n)
      (error "Number is negative")
      (* (expt 10 (find-number-of-nulls n)) (helper n 0)))
  )




(= (sort-n 1714) 7411)
(= (sort-n 123450) 543210)
(= (sort-n 123405) 543210)
(= (sort-n 123045) 543210)
(= (sort-n 120345) 543210)
(= (sort-n 102345) 543210)
(= (sort-n 8910) 9810)
(= (sort-n 321) 321)
(= (sort-n 29210) 92210)
(= (sort-n 1230) 3210)
(= (sort-n 55345) 55543)
(= (sort-n 14752) 75421)
(= (sort-n 329450) 954320)
(= (sort-n 9125) 9521)
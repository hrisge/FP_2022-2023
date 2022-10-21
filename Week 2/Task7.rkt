#lang racket

(define (count-occurrences n d)
  (define (helper counter cut-number)
    (cond
      [(zero? cut-number) counter]
      [(= (remainder cut-number 10) d) (helper (add1 counter) (quotient cut-number 10))]
      [else (helper counter (quotient cut-number 10))]
    )
  )

  (cond
    [(or (negative? n) (negative? d)) (error "n and d must be possitive")]
    [(and (zero? n) (zero? d)) 1]
    [(and (zero? n) (not (zero? d))) 0]
    [else (helper 0 n)]
)
  )


(= (count-occurrences 121 1) 2)
(= (count-occurrences 20 1) 0)


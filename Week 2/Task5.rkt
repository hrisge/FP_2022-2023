#lang racket

(require racket/trace)
(require math/number-theory)

(define (amicable? a b)
  (define (helper result current-number)
    (cond
      [(= current-number a) result]
      [(divides? current-number a) (helper (+ result current-number) (add1 current-number))]
      [else (helper result (add1 current-number))]
      )
    )

  (if(or (negative? a) (negative? b))
     (error "Interval must be possitive numbers")
     (= (helper 0 1) b)
  )
  )



(equal? (amicable? 200 300) #f)
(equal? (amicable? 220 284) #t)
(equal? (amicable? 284 220) #t)
(equal? (amicable? 1184 1210) #t)
(equal? (amicable? 2620 2924) #t)
(equal? (amicable? 6232 6368) #t)
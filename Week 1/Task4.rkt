#lang racket

(define (growing-plant up-speed down-speed desired-height)
  (define (helper up-speed down-speed desired-height starter counter)
    (if (>= (+ starter up-speed) desired-height) 
      (add1 counter)
      (helper up-speed down-speed desired-height (- (+ starter up-speed) down-speed) (add1 counter))
      )
    )
  (cond
    [(and (>= down-speed up-speed) (> desired-height up-speed)) error "Plant will never reach desired height"]
    [(negative? up-speed) error "Plant will never reach desired height"]
    [else (helper up-speed down-speed desired-height 0 0)] 
  )
  )



(= (growing-plant 5 2 5) 1)
(= (growing-plant 5 2 6) 2)
(= (growing-plant 10 9 4) 1)
(= (growing-plant 100 10 910) 10)
#lang racket

(define (snail desired-height day-distance night-distance)
  (define (helper starter counter)
    (if (>= (+ starter day-distance) desired-height)
        (add1 counter)
        (helper (- (+ starter day-distance) night-distance) (add1 counter)
        )
    )
  )
  (cond
    [(and (>= night-distance day-distance) (> desired-height day-distance)) error "Snail will never reach the end"]
    [(negative? day-distance) error "Snail will never reach the end"]
    [else (helper 0 0)]
    )
  )

(= (snail 3 2 1) 2)
(= (snail 10 3 1) 5)
(= (snail 10 3 2) 8)
(= (snail 100 20 5) 7)
(= (snail 5 10 3) 1)
#lang racket

(define (max-multiple d b)
  (define (helper num)
    (cond
      [(= 1 num) num]
      [(and (zero? (remainder num d)) (<= num b)) num]
      [else (helper (sub1 num))]
    )
    )
  
  (if (or (negative? d) (negative? b))
      (error "d and b must be possitive")
      (helper b)
    )
  )


(= (max-multiple 2 7) 6)
(= (max-multiple 3 10) 9)
(= (max-multiple 7 17) 14)
(= (max-multiple 10 50) 50)
(= (max-multiple 37 200) 185)
(= (max-multiple 7 100) 98)
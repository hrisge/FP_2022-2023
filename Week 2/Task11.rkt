#lang racket

(require math/number-theory)

(define (nth-cubic n)
  (define (helper counter num1 num2)
    (cond
      [(and (prime? (- (* num2 num2 num2) (* num1 num1 num1))) (= n (add1 counter))) (- (* num2 num2 num2) (* num1 num1 num1))]
      [(prime? (- (* num2 num2 num2) (* num1 num1 num1))) (helper (add1 counter) (add1 num1) (add1 num2))]
      [else (helper counter (add1 num1) (add1 num2))]
        )
    )
    (if (or (negative? n) (zero? n))
        (error "number must be possitive")
        (helper 0 1 2)
    )
  )

(= (nth-cubic 1) 7)
(= (nth-cubic 4) 61) ; 61 is the 4th cubic prime number
(= (nth-cubic 50) 55897) ; 55897 is the 50th cubic prime number
(= (nth-cubic 100) 283669)
(= (nth-cubic 200) 1570357)
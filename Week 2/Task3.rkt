#lang racket

(require math/number-theory)
(require racket/trace)

(define (sum-prime-divs-rec n)
  (define (helper counter)
    (cond
      [(zero? counter) 0]
      [(= counter 1) 0]
      [(and (prime? counter) (zero? (remainder n counter))) (+ counter (helper (sub1 counter)))]
      [else (helper (sub1 counter))]
       )
    )

  (if (negative? n)
     (error "n was negative")
     (helper n)
  )
)
(trace sum-prime-divs-rec)



(= (sum-prime-divs-rec 0) 0)
(= (sum-prime-divs-rec 6) 5) ; 2 + 3
(= (sum-prime-divs-rec 18) 5) ; 2 + 3
(= (sum-prime-divs-rec 19) 19)
(= (sum-prime-divs-rec 45136) 53)
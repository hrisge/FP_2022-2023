#lang racket

(define (automorphic? n)
  (define (length-of-number n counter)
    (if(zero? n)
       counter
       (length-of-number (quotient n 10) (add1 counter))
       )
    )
  (if (or (zero? n) (negative? n))
    (error "n was not natural")
    (= (remainder (* n n) (expt 10 (length-of-number n 0))) n)
    )
  )



(equal? (automorphic? 3)#f)
(equal? (automorphic? 10)#f)
(equal? (automorphic? 5)#t)
(equal? (automorphic? 25)#t)
(equal? (automorphic? 76)#t) 
(equal? (automorphic? 890625)#t) 
(equal? (automorphic? 625)#t) 
(equal? (automorphic? 36) #f)
(equal? (automorphic? 11) #f)
#lang racket

(define (my-list-ref xs i)
  (if (and (not (negative? i)) (list? (drop xs i)))
      (car (drop xs i))
      (error "Invalid index!")
      )
  )
  


(= (my-list-ref '(1 2 3) 0) 1)
(= (my-list-ref '(1 2 3) 1) 2)
(equal? (my-list-ref '("Hello" 2 ("nested list")) 0) "Hello")
(my-list-ref '(1 2 3) -100) ; error: Invalid index!
#lang racket

(require racket/trace)

(define (p n)
  (define (helper counter result)
    (if (or (= n 1) (= counter n))
      result
      (helper (add1  counter) (+ result (add1 (* counter 3))))
    ))
  
  (if (not (positive? n))
      (error "N is not a possitive number")
      (helper 1 1)
      ))


(= (p 1) 1)
(= (p 2) 5)
(= (p 3) 12)
(= (p 4) 22)
(= (p 5) 35)
(= (p 6) 51)
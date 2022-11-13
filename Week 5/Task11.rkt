#lang racket

(define (concat-proc xs ys)
  (append xs ys))

(define (concat-rec xs ys)
  (define (helper results leftovers)
    (if (empty? leftovers)
        results
        (helper (append results (list (car leftovers))) (cdr leftovers))))

  (helper xs ys)
  )



; using a predefined procedure
(equal? (concat-proc '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

; using a linearly iterative process
(equal? (concat-rec '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
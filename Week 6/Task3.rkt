#lang racket

(require racket/trace)

(define (num-bigger-elements xs)
  (define (helper leftovers results)
     (if (empty? leftovers)
         results
         (helper (cdr leftovers) (append results (list (cons (car leftovers) (length (filter (λ (x) (< (car leftovers) x)) xs))))))
         )
     )
  (helper xs null)
  )

(equal? (num-bigger-elements '(5 6 3 4)) '((5 . 1) (6 . 0) (3 . 3) (4 . 2)))
(equal? (num-bigger-elements '(1 1 1)) '((1 . 0) (1 . 0) (1 . 0)))
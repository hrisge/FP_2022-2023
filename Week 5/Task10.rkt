#lang racket

(require racket/trace)

(define (insert-at x i xs)
  (define (helper results leftovers i)
    (if (zero? i)
        (append (append results (list x)) leftovers)
        (helper (append results (list (car leftovers))) (cdr leftovers) (sub1 i)))
        )


    (helper null xs i)
    )


(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
(equal? (insert-at 7 0 '(1 2 3)) '(7 1 2 3))
(equal? (insert-at 7 1 '(1 2 3)) '(1 7 2 3))
(equal? (insert-at 7 3 '(1 2 3)) '(1 2 3 7))
(insert-at 7 4 '(1 2 3)) ; error: Invalid index!
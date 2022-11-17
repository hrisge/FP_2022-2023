#lang racket

(require racket/trace)

(define (my-cartesian-product xs ys)
  (define (helper-one xs-twos results)
    (define (helper-two ys-twos result-twos)
      (if (empty? ys-twos)
          result-twos
          (helper-two (cdr ys-twos) (append result-twos (list (cons (car xs-twos) (car ys-twos)))))
          ))

    (if (empty? xs-twos)
        results
        (helper-one (cdr xs-twos) (helper-two ys results))
        )
    )
  (helper-one xs null)
  
  )

(equal? (my-cartesian-product '(1 2) '(3 4)) '((1 . 3) (1 . 4) (2 . 3) (2 . 4)))
(equal? (my-cartesian-product '(1 2 3 4 5) '(6 7 8)) '((1 . 6) (1 . 7) (1 . 8) (2 . 6) (2 . 7) (2 . 8) (3 . 6) (3 . 7) (3 . 8) (4 . 6) (4 . 7) (4 . 8) (5 . 6) (5 . 7) (5 . 8)))
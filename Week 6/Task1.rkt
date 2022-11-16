#lang racket


(define (where xs fs)
  (define (helper leftovers results f)
    (if (empty? leftovers)
        results
        (helper (cdr leftovers) (if (f (car leftovers))
                                    (append results (list (car leftovers)))
                                    results) f)))

  (if (empty? fs)
      xs
      (where (helper xs null (car fs)) (cdr fs))
  ))

(equal? (where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) '(6 8 10)) ; all even numbers greater than5
(equal? (where '(3 4 5 7) (list even? (lambda (x) (> x 5)))) '()) ; no numbers are even and greater than 5
(equal? (where '() (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '())
(equal? (where '(1 2 3 4 5 6 7 8 9 10 11 13 15) (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '(5 7 9 11 13 15))
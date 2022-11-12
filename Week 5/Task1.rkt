#lang racket

(require racket/trace)

(define (remove-all-no-proc el xs)
  (define (helper results leftovers)
    (cond
      [(or (empty? leftovers) (not (list? (member el leftovers)))) (append results leftovers)]
      [(not (equal? el (car leftovers))) (helper (append results (list (car leftovers))) (cdr leftovers))]
      [else (helper results (cdr leftovers))]
      ))

  (if (list? (member el xs))
      (helper null xs)
      xs
      )
  )

(define (remove-all-proc el xs)
  (remq* (list el) xs)
  )


; without using a predefined procedure
(equal? (remove-all-no-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-no-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-no-proc 1 '(1)) '())
(equal? (remove-all-no-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-no-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))

; using a predefined procedure
(equal? (remove-all-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-proc 1 '(1)) '())
(equal? (remove-all-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))


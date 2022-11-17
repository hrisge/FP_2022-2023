#lang racket
(require racket/trace)

(define (persistence n)
  (define (helper result leftover)
    (if (zero? leftover)
        result
        (helper (* result (remainder leftover 10)) (quotient leftover 10))))

  (define (helper-two result curr-element)
    (if (< curr-element 10)
        result
        (helper-two (append result (list (helper 1 curr-element))) (helper 1 curr-element))))

  (cond
    [(negative? n) (error "N is negative!")]
    [(< n 10) (cons (list n) 1)]
    [else (cons (helper-two null n) (length (helper-two null n)))]

    )
  )


(equal? (persistence 39) '((27 14 4) . 3))
(equal? (persistence 126) '((12 2) . 2))
(equal? (persistence 4) '((4) . 1))
(equal? (persistence 999) '((729 126 12 2) . 4))
#lang racket

(define (longest-ascending-sub xs)
  (define (helper current-longests buffs leftovers)
    (cond
      [(empty? leftovers) (if (> (length current-longests) (length buffs))
                             current-longests
                             buffs)]
      [(or (empty? buffs) (<= (last buffs) (car leftovers))) (helper current-longests (append buffs (list (car leftovers))) (cdr leftovers))]
      [else (if (> (length current-longests) (length buffs))
                (helper current-longests null leftovers)
                (helper buffs null leftovers)
                )]
      )
    )
  (helper null null xs)

  )


(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))
#lang racket

(require math/number-theory)

(define (n-odd-number n)
  (cond
    [(negative? n) (error "n is negative")]
    [(zero? n) -1]
    [else (+ 2 (n-odd-number (sub1 n)))]
    )
  )

(define (calc-series-sum x n)
  (define (helper counter denominator)
    (cond
      [(> counter n) 0]
      [(odd? counter)
        (+
         (-
          (/ (expt (* 2 x) counter) (* denominator (n-odd-number counter))))
         (helper (add1 counter) (* denominator (n-odd-number counter))))]
      [else
       (+
        (/ (expt (* 2 x) counter) (* denominator (n-odd-number counter)))
         (helper (add1 counter) (* denominator (n-odd-number counter))))]))


  (if (or (negative? x) (negative? n))
      (error "N or X is negative")
      (helper 1 1)
      )
  )




(calc-series-sum 1 0) ; -2
(calc-series-sum 1 1) ; -2/3
(calc-series-sum 1 2) ; -1 1/5
(calc-series-sum 1 3) ; -1 1/21
(calc-series-sum 1 4) ; -1 11/135
(calc-series-sum 1 5) ; -1 29/385
(calc-series-sum 1 6) ; -1 937/12285


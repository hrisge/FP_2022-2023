#lang racket

(require math/number-theory)
(require racket/trace)

(define (find-sum a b n)
  (define (helper counter)
   (cond
     [(<= counter (- n 3)) (+ (* 3 b (expt 2 counter)) (helper (add1 counter)))]
     [(= counter (- n 2)) (+ (* 2 b (expt 2 counter)) (helper (add1 counter)))]
     [(= counter (sub1 n)) (* b (expt 2 counter))]
     )
    )
  (trace helper)
  (+ (* 3 a) (helper 0))
)


(= (find-sum 0 2 10) 3578) ; 510 + 1022 + 2046
(= (find-sum 5 3 5) 174) ; 26 + 50 + 98
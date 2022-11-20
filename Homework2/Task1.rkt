#lang racket

(require racket/trace)


(define (pair-compose fs xs)
  (define (helper result leftover-fs leftover-xs)
    (cond
      [(and (empty? leftover-fs) (empty? leftover-xs)) result]
      [(and (= 1 (length leftover-fs)) (= 1 (length leftover-xs))) (helper (append result (λ (x) ((first leftover-fs) (first leftover-xs) x))) (cdr leftover-fs) (cdr leftover-xs))]
      [else (helper (append result (list (λ (x) ((first leftover-fs) (first leftover-xs) ((second leftover-fs) (second leftover-xs) x))))) (cddr leftover-fs) (cddr leftover-xs))]      
      ))
  (trace helper)

  

  (define (helper-two res functions n)
    (if (empty? functions)
        res
        (helper-two (+ res ((car functions) n)) (cdr functions) n)))
  (trace helper-two)

  (λ (x) (helper-two 0 (helper null fs xs) x))
  )
(trace pair-compose)





 ;((f1 x1).(f2 x2) y) + ((f3 x3).(f4 x4) y) + ... + ((fn-1 xn-1).(fn xn) y)

(define fs (list
 *
 (λ (x y) (* x x x y))
 (λ (x y) (+ x 1 y))
 (λ (x y) (- x (+ 1 y)))
 (λ (x y) (* x y 2))))

(define xs '(1 2 3 4 5))
(pair-compose fs xs ) 5
(= ((pair-compose fs xs) 5) 92)
; ((* 1).(* 8) 5) + ((+ 4).(- 3) 5) + ((* 10).id 5) = 40 + 2 + 50 = 92
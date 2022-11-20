#lang racket

(require racket/trace)

(define  (woodcutters xs)
  (define (without-zeros result leftovers)
    (cond
      [(null? leftovers) result]
      [(zero? (cdar leftovers)) (without-zeros result (cdr leftovers))]
      [else (without-zeros (append result (list (car leftovers))) (cdr leftovers))]))
  
  (define (helper fallen-trees leftover-xs)
    (cond
      
      [(null? leftover-xs) (length (without-zeros null fallen-trees))]

      [(null? fallen-trees) (helper (append fallen-trees (list (cons (- (caar leftover-xs) (cdar leftover-xs)) (caar leftover-xs)))) (cdr leftover-xs))]
      
      [(and (not (null? fallen-trees))
            (and (zero? (cdr (last fallen-trees)))
                 (> (- (caar leftover-xs) (cdar leftover-xs)) (car (last fallen-trees)))))
       (helper (append (without-zeros null fallen-trees) (list (cons (- (caar leftover-xs) (cdar leftover-xs)) (caar leftover-xs)))) (cdr leftover-xs))]


       [(and
         (not (null? fallen-trees))
         (not (zero? (cdr (last fallen-trees))))
         (> (- (caar leftover-xs) (cdar leftover-xs)) (cdr (last (without-zeros null fallen-trees)))))
       (helper (append (without-zeros null fallen-trees) (list (cons (- (caar leftover-xs) (cdar leftover-xs)) (caar leftover-xs)))) (cdr leftover-xs))]
       
      [(or (null? (cdr leftover-xs))
           (and (< (cdr (last (without-zeros null fallen-trees))) (caar leftover-xs))
                (< (+ (caar leftover-xs) (cdar leftover-xs)) (car (second leftover-xs)))))
       (helper (append (without-zeros null fallen-trees) (list (cons (caar leftover-xs) (+ (caar leftover-xs) (cdar leftover-xs))))) (cdr leftover-xs))]

      [else (helper (append (without-zeros null fallen-trees) (list (cons (caar leftover-xs) 0))) (cdr leftover-xs))]
      )
    )
  (helper null xs)


  )

(equal? (woodcutters '((1 . 2) (2 . 1) (5 . 10) (10 . 9) (19 . 1))) 3)
(equal? (woodcutters '((1 . 2) (2 . 1) (5 . 10) (10 . 9) (20 . 1))) 4)
(equal? (woodcutters '((10 . 4) (15 . 1) (19 . 3) (20 . 1))) 4)


(equal? (woodcutters '((1 . 7) (3 . 11) (6 . 12) (7 . 6) (8 . 5) (9 . 11)
  (16 . 10) (22 . 2) (23 . 3) (25 . 7) (27 . 3) (34 . 5)
 (35 . 10) (37 . 3) (39 . 4) (40 . 5) (41 . 1) (44 . 1) (47 . 7)
 (48 . 11) (50 . 6) (52 . 5) (57 . 2) (58 . 7) (60 . 4) (62 . 1)
 (67 . 3) (68 . 12) (69 . 8) (70 . 1) (71 . 5) (72 . 5)
 (73 . 6) (74 . 4) ) ) 10)

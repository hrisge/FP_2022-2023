#lang racket

(require racket/trace)

(define (repeater str)  
  (λ (count glue)
    (if (= count 1)
     str
     (string-append str glue ((repeater str) (sub1 count) glue))
  )))
(trace repeater)

(equal? ((repeater "I love Racket") 3 " ") "I love Racket I love Racket I love Racket")
(equal? ((repeater "Quack") 5 "!") "Quack!Quack!Quack!Quack!Quack")
#lang racket

(require racket/trace)

(define (longest s1 s2)
  (define (helper curr-char result)
    (cond
      [(equal? (char->integer curr-char) 123) result]
      [(or (list? (member curr-char s1)) (list? (member curr-char s2))) (helper (integer->char (add1 (char->integer curr-char))) (append result (list curr-char)))]
      [else (helper (integer->char (add1 (char->integer curr-char))) result)]
      )
    )

  (if (or (not (list? s1)) (not (list? s2)))
      (longest (string->list s1) (string->list s2))
      (list->string (helper #\a null))
      )
  )


(equal? (longest "xyaabbbccccdefww" "xxxxyyyyabklmopq") "abcdefklmopqwxy")
(equal? (longest "abcdefghijklmnopqrstuvwxyz" "abcdefghijklmnopqrstuvwxyz") "abcdefghijklmnopqrstuvwxyz")
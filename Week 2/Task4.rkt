#lang racket

(require racket/trace)

(define (rev n)
  (define (helper leftover result)
    (if (zero? leftover)
        result
        (helper (quotient leftover 10) (+ (* result 10) (remainder leftover 10)))
        )
    )
  (if (negative? n)
      (error "n has to be non-negative")
      (helper n 0)
      )
  )

(define (palindrome? n)
  (= n (rev n))
  )

(define (num-palindromes-rec a b)
  (cond
    [(or (negative? a) (negative? b)) (error "Interval must be possitive numbers")]
    [(< b a) (num-palindromes-rec b a)]
    [(and (palindrome? a) (not (= a b))) (add1 (num-palindromes-rec (add1 a) b))]
    [(and (palindrome? a) (= a b)) 1]
    [(and (not (palindrome? a)) (not (= a b))) (num-palindromes-rec (add1 a) b)]
    [else 0]
    )
  )

(define (num-palindromes-iter a b)
  (define (helper new-start new-finish result)
    (cond
      [(> new-start new-finish) result]
      [(palindrome? new-start) (helper (add1 new-start) new-finish (add1 result))]
      [else (helper (add1 new-start) new-finish result)]
      )
    )

  (if (or (negative? a) (negative? b))
      (error "Interval must be possitive numbers")
      (helper (min a b) (max a b) 0)
      )
  )

(= (num-palindromes-rec 1 101) 19)
(= (num-palindromes-rec 1 100) 18)
(= (num-palindromes-rec 100 1) 18)

(= (num-palindromes-iter 1 101) 19)
(= (num-palindromes-iter 1 100) 18)
(= (num-palindromes-iter 100 1) 18)


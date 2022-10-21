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
  (define (helper cnt current-number)
    (cond
      [(and (= current-number b) (palindrome? current-number)) (add1 cnt)]
      [(and (= current-number b)) cnt]
      [(palindrome? current-number) (helper (add1 cnt) (add1 current-number))]
      [else (helper cnt (add1 current-number))]
      )
  )

  (cond
    [(or (negative? a) (negative? b)) (error "Interval must be possitive numbers")]
    [(< b a) (num-palindromes-rec b a)]
    [else (helper 0 a)]
    )
  )

(= (num-palindromes-rec 1 101) 19)
(= (num-palindromes-rec 1 100) 18)
(= (num-palindromes-rec 100 1) 18)

(= (num-palindromes-iter 1 101) 19)
(= (num-palindromes-iter 1 100) 18)
(= (num-palindromes-iter 100 1) 18)


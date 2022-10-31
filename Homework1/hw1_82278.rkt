#lang racket

(require math/number-theory)

;Task 1
(define (sum-digits number result)
  (if (zero? number)
      result
      (sum-digits (quotient number 10) (+ result (remainder number 10)))
      )  
  )


(define (count-specials k a b)
  (cond
    [(> a b) 0]
    [(and (divides? k a) (divides? k (sum-digits a 0))) (add1 (count-specials k (add1 a) b))]
    [else (count-specials k (add1 a) b)]
  )
  )

(count-specials 3 3 9)
(count-specials 5 10 100)
(count-specials 8 100 200)
(count-specials 15 1000 2000)


;Task3

(define (number-len n)
  (if (zero? n)
      0
      (add1 (number-len (quotient n 10)))
      )
  )

(define (max-rot n)  

  (define (helper new-number current-biggest saved-numbers)
    ;The functions below are used in purpose of more readable code and not having enormous lines.
    (define (saved-numbers-front saved-numbers-func)
      (quotient new-number (expt 10 (- (number-len n) saved-numbers-func))))
  
    (define (saved-numbers-multiplied saved-numbers-func)
      (* (saved-numbers-front saved-numbers-func) (expt 10 (- (number-len n) saved-numbers-func))))

    (define (saved-numbers-all saved-numbers-func)
      (+ (saved-numbers-multiplied saved-numbers-func) (* 10 (remainder new-number (expt 10 (sub1 (- (number-len n) saved-numbers-func)))))))

    (define (result saved-numbers-func)
      (+ (saved-numbers-all saved-numbers-func) (remainder (quotient new-number (expt 10 (sub1 (- (number-len n) saved-numbers-func)))) 10)))

    (define (zeros-in-front)
      (- (number-len n) (number-len new-number)))
    ;Those functions end here. The code could be done without them but it will be a lot harder to be read from a human.
      
    (cond
      [(= (sub1 (number-len n)) saved-numbers) current-biggest]
      [(zero? (zeros-in-front)) (if (> (result saved-numbers) current-biggest)
                                                      (helper (result saved-numbers) (result saved-numbers) (add1 saved-numbers))
                                                      (helper (result saved-numbers) current-biggest (add1 saved-numbers)))]
      [(<= (zeros-in-front) saved-numbers) (if (> (result (- saved-numbers (zeros-in-front))) current-biggest)
                                                      (helper (- saved-numbers (zeros-in-front)) (- saved-numbers (zeros-in-front)) (add1 saved-numbers))
                                                      (helper (- saved-numbers (zeros-in-front)) current-biggest (add1 saved-numbers)))]
      [else (if (> (* new-number 10) current-biggest)
                (helper (* new-number 10) (* new-number 10) (add1 saved-numbers))
                (helper (* new-number 10) current-biggest (add1 saved-numbers)))]
      ))

  (if (negative? n)
      (error "n is negative")
      (helper n n 0))
  )

(max-rot 56789)
(max-rot 12490)
(max-rot 38458215)
(max-rot 195881031)
(max-rot 896219342)
(max-rot 69418307)
(max-rot 69418307)
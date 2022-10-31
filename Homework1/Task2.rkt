#lang racket

(require math/number-theory)
(require racket/trace)

(define (number-len n)
  (if (zero? n)
      0
      (add1 (number-len (quotient n 10)))
      )
  )

(define (max-rot n)  

  (define (helper new-number current-biggest saved-numbers)
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


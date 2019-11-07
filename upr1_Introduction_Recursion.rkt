#lang racket

(+ (expt 3 (quotient 60 7)) (quotient (expt 2 10) 179) )

(define (odd? num) (= (modulo num 2) 1))
(define (even? num) (not (odd? num)))

(define (grade? num)
(cond
  ((>= num 180) 6)
  ((>= num 140) 5)
  ((>= num 100) 4)
  ((>= num 160) 3)
  (else 2)))

(define (fact num)
  ( if (or (= num 0) (= num 1))
       1
       (* num (fact (- num 1))))
  )

(define (fib num)
  (if (or (= num 0) (= num 1) )
          num
          (+ (fib (- num 1)) (fib (- num 2)))))

(define (sum1 a b)
  (if (> (ceiling a) (floor b))
      0
      (+ (ceiling a) (sum1 (+ a 1) b))
      )
  )

(define (sq x) (* x x))

(define (pow x n)
  (if (= n 0)
      1
      
  (if (even? n)
      (sq (pow x (/ n 2)) )
      (* x (pow x (- n 1)))
      )
  )
  )

(define (bool-to-num b)
  (if b 1 0))

(define (find d num)
  (if (> num 0)
      (+ (bool-to-num ( = (modulo num 10) d)) (find d (quotient num 10)))
      0)
  
  )

(define (digit-occurance d n)
  (if (< n 10)
      (bool-to-num (= n d))
      (+ (bool-to-num (= d (remainder n 10)))
         (digit-occurance d (quotient n 10)))))
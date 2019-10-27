#lang racket

(define (1+ n) (+ 1 n))
(define (2+ n) (+21 n))
(define (sq n) (* n n))
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))


(define (sum-step a b next) 
  (if (> a b)
        0
        (+ a (sum-step (next a) b next)))
  )

(define (sum-term a b term) 
  (if (> a b)
        0
        (+ (term a) (sum-term (+ 1 a) b term)))
  )

(define (sum a b term next) 
  (if (> a b)
        0
        (+ (term a) (sum (next a) b term next)))
  )


(define (my-exp m x)
  (sum 0 m (lambda (n) (/ (expt x n) (fact n))) 1+)
  )

(define (my-sin m x)
  (sum 0 m (lambda (n) (* (expt -1 n)(/ (expt x (+ 1 (* 2 n))) (fact (+ 1 (* 2 n)))))) 1+)
  )

(define (my-cos m x)
  (sum 0 m (lambda (n) (* (expt -1 n)(/ (expt x (* 2 n)) (fact (* 2 n))))) 1+)
  )

;(define (sprod p x)
;  (sum 1 p (lambda (n) (* (expt -1 n)(/ (expt x n) fact(n)))) 2+)
;  )

(define (product a b term next) 
  (if (> a b)
        1
        (* (term a) (product (next a) b term next)))
  )


(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next)))
  )


(define (fact2 n)
  (accumulate * 1 1 n (lambda (x) x) 1+))

(define (sum2 a b term next)
  (accumulate + 0 a b term next))

(define (product2 a b term next)
  (accumulate * 1 a b term next))

(define (accumulate-i operation null-value a b term next)
  (define (help i result)
    (if (> i b)
        result
        (help (next i) (operation result (term i)))))
  (help a null-value))

(define (bool-to-num b)
  (if b 1 0))

(define(palindrome2 n)
  (define (help n pos)
    (if (<= pos (quotient (count-digits n) 2))
        (if (= (remainder (quotient n (expt 10 (- (count-digits n) pos))) 10 )
               ( quotient (remainder n (expt 10 pos))(expt 10 (- pos 1)) ) )
            (help n (+ pos 1))
            #f)
        #t))
  (help n 1))

(define (count-digits num)
  (define (help num counter)
    (if (= num 0)
         counter
        (help (quotient num 10) (+ counter 1))))
  (if (= num 0)
         1
         (help (abs num) 0)))

(define (count-palindromes a b)
  (accumulate + 0 a b (lambda (n) (bool-to-num (palindrome2 n))) 1+)
  )

(define (prime1 n)
  (define (help n del)
    (if (< del n)
        (if (= (remainder n del) 0)
            #f
            (help n (+ del 1)))
        #t))
  (help n 2))

(define (prime2 x)
  (accumulate + 0 2 x (lambda (n) (bool-to-num (= (remainder x n) 0))) 1+)
  )

(define (prime? x)
  (= (prime2 x) 1))
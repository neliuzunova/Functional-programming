#lang racket

(define (1+ n) (+ 1 n))
(define (sq n) (* n n))

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
  (sum 0 m (lambda (n) (* (expt (-1) n)(/ (expt x (+ 1 (* 2 n))) (fact (+ 1 (* 2 n)))))) 1+)
  )

(define (my-scos m x)
  (sum 0 m (lambda (n) (* (expt (-1) n)(/ (expt x (* 2 n)) (fact (* 2 n))))) 1+)
  )

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



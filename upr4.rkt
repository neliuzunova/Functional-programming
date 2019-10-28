#lang racket
(define (1+ n) (+ 1 n))

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next)))
  )

(define (exists? pred? a b)
  (accumulate (lambda (x y) (or x y))
                (or) a b pred? 1+)
  )

(define ((forall? pred? a b))
  (not (exists? (lambda (k) (not (pred? k))) a b))
  )

;spisaci

(define l1 '(1 2 3 4 5 6 7 8))
(define l2 '(80 70 60 50 40 30 20 10))

(define (lenght L)
  (if (null? L)
      0
      (1+ (lenght (cdr L)))))

(define (sum L)
  (if (null? L)
      0
      (+ (car L) (sum (cdr L)))))

(define (last L)
  (if (= (lenght L) 1)
      (car L)
      (last (cdr L))))

(define (new-append L M)
  (if (null? L)
      M
      (cons (car L) (new-append (cdr L) M)))
  )

(define (push-back x L)
  (if (null? L)
      (list x)
      (new-append (list (car L)) (push-back x (cdr L))))
  )

(define (member? x L)
  (if (null? L)
      #f
      (if (eq? x (car L))
          #t
          (member? x (cdr L))))
  )

(define (from-to a b)
  (if (> a b)
      (list a)
      (push-back (+ a 1) L))
  )
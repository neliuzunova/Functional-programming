#lang racket

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next)))
  )

(define (accumulate-i operation null-value a b term next)
  (define (help i result)
    (if (> i b)
        result
        (help (next i) (operation result (term i)))))
  (help a null-value))

(define (foldl op v l)
  (if (null? l)
      v
      (foldl op (op v (car l)) (cdr l))))

(define (foldr op nv l)
  (if (null? l)
      nv
      (op (car l) (foldr op nv (cdr l)))))

(define (flip f)
  (λ (x y) (f y x))
  )

(define (twice f x) (f (f x)))
(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
        (if (= n 1)
            (λ (x) (f x))
            (compose f (repeated f (- n 1))))
  )

(define (append L M)
  (if (null? L)
      M
      (cons (car L) (append (cdr L) M)))
  )

(define (push-back x L)
  (if (null? L)
      (list x)
      (append (list (car L)) (push-back x (cdr L))))
  )

(define (member? x L)
  (if (null? L)
      #f
      (if (eqv? x (car L))
          #t
          (member? x (cdr L))))
  )

(define (reverse L)
  (if (= (lenght L) 1)
      L
      (append (reverse (cdr L)) (list (car L))))
  )

(define (map f L)
  (if (null? L)
      L
      (append (list (f (car L))) (map f (cdr L))))
  )

(define (filter pred? L)
  (if (null? L)
      L
      (if (pred? (car L))
          (append (list (car L)) (filter pred? (cdr L)))
          (filter pred? (cdr L)))
      )
  )

(define (find d L)
  (if (null? L)
      0
      (if (eqv? d (car L))
          (+ 1 (find d (cdr L)))
          (find d (cdr L))))
  )

(define (all? p? l)
  (if (null? l)
      #t
      (if (p? (car l))
          (all? p? (cdr l))
          #f)))

(define (any? p? l)
  (if (null? l)
      #f
      (or (p? (car l))
          (any? p? (cdr l)))))

(define (next-look-and-say l)
  (define (count x l1)
    (if (null? l1)
        0
        (if (= (car l1) x)
            (+ 1 (count x (cdr l1)))
            0)))
  (if (null? l)
      l
      (let ((c (count (car l) l)))
           (cons c
                 (cons (car l)
                       (next-look-and-say (drop l c)))))))

(define (quicksort l)
  (if (or (null? l) (null? (cdr l)))
      l
      (let ((pivot (car l)))
        (append
          (quicksort (filter (lambda (x) (<= x pivot)) (cdr l)))
          (list pivot)
          (quicksort (filter (lambda (x) (> x pivot)) l))))))

(define (list? x)
  (or (null? x)
      (and (pair? x)
           (list? (cdr x)))))
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))
(define (flatten dl)
  (cond ((null? dl) '())
        ((atom? dl) (list dl))
        (else (append (flatten (car dl))
                      (flatten (cdr dl))))))

(define (prime2 x)
  (accumulate + 0 2 x (lambda (n) (bool-to-num (= (remainder x n) 0))) 1+)
  )

(define (prime? x)
  (= (prime2 x) 1))

(define (reverse-num n)
  (define (help n new)
    (if (= n 0)
        new
    (help (quotient n 10) (+ (* new 10) (remainder n 10))
    )))
  (help n 0))
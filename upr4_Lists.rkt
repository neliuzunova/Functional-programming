#lang racket
(define (1+ n) (+ 1 n))

(define l1 '(1 2 3 4 5 6 7 8))
(define l2 '(0 (12 13) (21 22)))

;(list-tail l1 3)
;(list-ref l1 3) 

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
#|
(require rackunit rackunit/text-ui)

(run-tests (test-suite "member? tests"
             (check-true (member? 5 l1))
             (check-true (member? 1 l1))
             (check-true (member? '() '(())))
             (check-true (member? '(21 22)  l2))
             (check-false (member? 5 '()))
             (check-false (member? '() l1))
             (check-false (member? '(21) l2))
             (check-false (member? '(21 22 23) l2))
             (check-false (member? '(0) l2)))
           'verbose)
|#
(define (from-to a b)
        (if (> a b)
             '()
            (append (list a) (from-to (+ a 1) b)))
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

(define (not-filter pred? L)
  (if (null? L)
      L
      (if (pred? (car L))
          (not-filter pred? (cdr L))
          (append (list (car L)) (not-filter pred? (cdr L)))
          )
      ))

(define (partition pred? L)
    (list (filter pred? L) (not-filter pred? L))
    )

(define (prime? n)
  (define (help n del)
    (if (< del n)
        (if (= (remainder n del) 0)
            #f
            (help n (+ del 1)))
        #t))
  (help n 2))

(define (scp L)
  (sum (map (λ (x) (* x (sqr x))) (filter prime? L)))
  )

(define (take n L)
  (if (= n 0)
      '()
      (append (list (car L)) (take (- n 1) (cdr L))))
  )

(define (drop n L)
    (if (= n 0)
        L
        (drop (- n 1) (cdr L)))
  )

(define (list-ref L n)
  (car (drop n L))
  )

(define (list-tail L n)
  (cdr (drop n L))
  )

(define (insert n x L)
  (append (push-back x (take n L)) (drop n L))
  )

(define (remove x L)
  (if (eqv?  x (car L))
      (cdr L)
      (append (list (car L))(remove x (cdr L))))
  )
; dont work for deep lists

(define (explode-digits n)
  (if (< n 10)
      (list n)
      (push-back (remainder n 10) (explode-digits (quotient n 10))))
  )

(define (digit-occurence d n)
  (define L (explode-digits n))
  (find d L)
  )

(define (find d L)
  (if (null? L)
      0
      (if (eqv? d (car L))
          (+ 1 (find d (cdr L)))
          (find d (cdr L))))
  )

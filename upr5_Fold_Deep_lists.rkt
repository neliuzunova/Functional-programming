#lang racket

(define l1 '(1 2 3 4 5 6 7 8))
(define l2 (reverse l1))

(define (maximum L)
  (if (null? (cdr L))
      (car L)
      (max (car L) (maximum (cdr L))))
  )

(define (remove x L)
  (if (eqv?  x (car L))
      (cdr L)
      (append (list (car L))(remove x (cdr L))))
  )

(define (selection-sort L)
  (if (null? L)
      L
      (cons (selection-sort (remove (maximum L) L )) (maximum L)))
  )

(define (take n L)
  (if (or (null? L)
          (= n 0))
      '()
      (cons (car L) (take (- n 1) (cdr L)))))

(define (drop n L)
  (if (or (null? L)
          (= n 0))
      L
      (drop (- n 1) (cdr L))))

(define (slice a b L)
  (take (+ (- b a) 1) (drop a L))
  )

(define (zip L M)
  (if (= (length L) 1)
      (cons (car L) (car M))
      (list (cons (car L) (car M)) (zip (cdr L) (cdr M))))
  )

(define (unique L)
  (if (null? L)
      L
      (let ((h (car L)))
        (cons h (unique (filter (lambda (x)
                                  (not (equal? x h)))
                                L))))))

(define (find d L)
  (if (null? L)
      0
      (if (eqv? d (car L))
          (+ 1 (find d (cdr L)))
          (find d (cdr L))))
  )

;sechenie
(define (intersection L M)
  (filter (lambda (x)
            (member x M))
          L))

;obedinenie
(define (union L M)
  (unique (flatten (append L M)))
  )

;razlika
(define (set-minus L M)
  (unique (filter (lambda (x)
            (not (member x M))) L))
  )

(define (chunk n L)
  (if (null? L)
      L
      (cons (take n L)
            (chunk n (drop n L)))))

(define (minimum less? L)
  (define (my-min a b)
    (if (less? a b) a b))

  (if (null? (cdr L))
      (car L)
      (my-min (car L) (minimum less? (cdr L)))))

(define (selection-sort2 less? L)
  (if (null? L)
      L
      (let ((m (minimum less? L)))
        (cons m (selection-sort2 less? (remove m L))))))


(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (count-atoms dl)
  (cond ((null? dl) 0)
        ((atom? dl) 1)
        (else (+ (count-atoms (car dl))
                 (count-atoms (cdr dl))))))

(define (flatten dl)
  (cond ((null? dl) '())
        ((atom? dl) (list dl))
        (else (append (flatten (car dl))
                 (flatten (cdr dl)))))
  )

(define (deep-foldr op nv term dl)
  (cond ((null? dl) nv)
        ((atom? dl) (term dl))
        (else (op (deep-foldr op nv term (car dl))
                  (deep-foldr op nv term (cdr dl))))))

(define (push-back x L)
  (append L (list x)))

(define (id x) x)

(define (deep-reverse DL)
  (deep-foldr push-back '() id DL))

(define (quicksort l)
  (if (or (null? l) (null? (cdr l)))
      l
      (let ((pivot (car l)))
        (append
          (quicksort (filter (lambda (x) (<= x pivot)) (cdr l)))
          (list pivot)
          (quicksort (filter (lambda (x) (> x pivot)) l))))))
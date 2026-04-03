#lang racket
;potoci
(define the-empty-stream '())
(define (cons-stream* h t) (cons h (delay t)))
(define head car)
(define (tail s) (force (cdr s)))
(define empty-stream? null?)

;spec form
(define-syntax delay
  (syntax-rules () ((delay x) (lambda () x))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t)
     (cons h (delay t)))))

(define (enum a b)
  (if (> a b)
      the-empty-stream
      (cons-stream a (enum (+ a 1) b))))

(define (first n s)
  (if (or (empty-stream? s) ( = n 0))
      '()
      (cons (head s) (first (- n 1) (tail s)))))

(define (from n)
  (cons-stream n (from(+ n 1))))

(define nats (from 0))

(define (repeat x)
  (cons-stream x (repeat x)))

(define ones (repeat 1))

(define (map-stream f s)
  (cons-stream (f (head s))
               (map-stream f (tail s))))

(define (filter-stream p? s)
  (if (p? (head s))
      (cons-stream (head s) (filter-stream p? (tail s)))
      (filter-stream p? (tail s))))

(define (zip-streams op s1 s2)
  (cons-stream (op (head s1) (head s2))
               (zip-streams (tail s1) (tail s2))))

;(define (map-stream f . streams)
;  (cons-stream (apply f (map head streams))
;               (apply map-stream f (map tail streams))))

(define (1+) (λ(x) (+ 1 x)))
(define ones* (cons-stream 1 ones*))
(define nats* (cons-stream 0 (map-stream 1+ nats*)))

;(define fibs* (map-stream fib nats))
(define (add-streams xs ys)
  (cons-stream (+ (head xs)
                  (head ys))
               (add-streams (tail xs)
                            (tail ys))))

(define fibs
  (cons-stream 0
    (cons-stream 1
      (add-streams fibs
                   (tail fibs)))))

(define (divides? d n) (= 0 (remainder n d)))
(define (sieve xs)
  (cons-stream (head xs)
               (sieve
                 (filter-stream
                   (lambda (y)
                     (not (divides?
                            (head xs)
                            y)))
                   xs))))
               
(define primes (sieve (from 2)))




;graphi
(define s1 '((1 . 2) (2 . 3) (3 . 4)))
(define id (λ(x) x))

(define (make-alist f keys)
  (map (lambda (x) (cons x (f x))) keys))

(define (keys alist) (map car alist))
(define (values alist) (map cdr alist))

#|(assoc <ключ> <асоциативен-списък>)
Ако <ключ> се среща сред ключовете на <асоциативен-списък>,
връща първата двойка (<ключ> . <стойност>)
Ако <ключ> не се среща сред ключовете, връща #f
Сравнението се извършва с equal
assv -> eqv?
assq -> eq?
|#

(define (del-assoc key alist)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist))

(define (add-assoc key value alist)
  (cons (cons key value) (del-assoc key alist)))
; delete old key and add new key

;any?
(define (search p l)
  (and (not (null? l))
       (or (p (car l)) (search p (cdr l)))))

(define (assq key al)
  (search (lambda (kv) (and (eq? (car kv) key) kv)) al))

;all?
(define (all? p l)
  (not (search (lambda (x) (not (p x))) l)))

;graps


(define vertices keys) 

(define (children v g)
  (cdr (assv v g)))

(define (edge? u v g)
  (memv v (children u g)))

(define (map-children v f g)
  (map f (children v g)))

(define (search-child v f g)
  (search f (children v g)))

(define (childless g)
  (filter (lambda (v) (null? (children v g))) (vertices g)))

(define (parents v g)
  (filter (lambda (u) (edge? u v g)) (vertices g)))

(define (symmetric? g)
  (all? (lambda (u)
          (all? (lambda (v) (edge? v u g))
                (children u g)))
        (vertices g)))
#|
(define (dfs-path u v g)
  (define (dfs-search path)
    (let ((current (car path)))
      (cond ((eqv? current v)
             (reverse path)
            ((memv current (cdr path)) #f)
            (else (search-child current
                                (lambda (w) (dfs-search (cons w path))) g)))
            )))
    (dfs-search (list u)))
|#

(define (extend path)
  (map-children (car path)
                (lambda (w) (cons w path)) g))

(define (remains-acyclic? path)
  (not (memv (car path) (cdr path))))

(define (extend-acyclic path)
  (filter remains-acyclic? (extend path)))

(define (bfs-path u v g)
  (define (extend path) ...)
  (define (extend-acyclic path) ...)
  (define (extend-level level)
    (apply append (map extend-acyclic level)))

(define (target-path path)
  (and (eqv? (car path) v) path))
  
(define (bfs-level level)
  (and (not (null? level))
       (or (search target-path level)
           (bfs-level (extend-level level)))))
(bfs-level (list (list u))))



(define g1 '((1 2 3) (2 3) (3 4 5) (4) (5 2 4 6) (6 2)))
(define gt '((1) (2 1 5 6) (3 1 2) (4 3 5) (5 3) (6 5)))

(define (indegree v g)
  (parents v g))

 (define (outdegree v g)
   (children v g))

(define (graph-transpose g)
  (list (map (λ(x) (cons x (indegree x g))) (vertices g)))
  )
  

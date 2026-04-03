#lang racket

(define l1 '((1 2 3) (4 5 6)))
(define l2 '((1 0) (0 0) (0 0)))

#|
(define (map f L)
  (if (null? L)
      L
      (append (list (f (car L))) (map f (cdr L))))
  )
|#
;transpose
(define (get-first-cow m)
  (map car m))

(define (del-first-cow m)
  (map cdr m))

(define (transpose m)
  (if (null? (car m))
      '()
      (cons (map car m) (transpose (map cdr m))))
  )

;mult
(define (mult-vect v1 v2)
  (apply + (map * v1 v2)))
(define (multiply m1 m2)
  (define m2t (transpose m2))
  (map (λ (row) (map (λ(cow) (mult-vect row cow)) m2t)) m1)
  )

;map
(define (map-matrix f m)
  (map (λ (row) (map f row)) m)
  )

;diagonal
(define (main-diagonal m)
  (if (null? m)
      '()
      (if (= (length (car m)) 1)
          (cons (caar m) '())
          (cons (caar m) (main-diagonal (del-first-cow (cdr m)))))
  ))

;trees
(define t1 '(1 (2 () ())
               (3 (4 () ())
                  (5 () ())))  )
(define t2 '(1 ()
               (3 ()
                  ()))  )
(define t3 '(2 (1 () ())
               (4 (3 () ())
                  (5 () ())))  )
(define t4 '(5 (1 () ())
               (3 (1 () ())
                  (1 () ())))  )

(define (tree? t)
  (or (null? t)
      (and (list t) (= (length t) 3))
      (tree? (cadr t))
      (tree? (caddr t))))

(define empty-tree '())
(define (make-tree root left right) (list root left right))

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define (height t)
  (if (empty-tree? t)
      0
      (+ 1 (max (height (left-tree t))
                (height (right-tree t)))))
  )

(define (level n t)
  (if (empty-tree? t)
      '()
  (if (= n 0)
      (root-tree t)
      (list (level (- n 1) (left-tree t))
            (level (- n 1) (right-tree t)))))
  )

(define (count-leaves t)
  (if (empty-tree? t)
      0
      (if (and (empty-tree? (left-tree t)) (empty-tree? (right-tree t)))
          1
          (+ (count-leaves (left-tree t))
          (count-leaves (right-tree t)))))
  )

(define (remove-leaves t)
  (if (empty-tree? t)
      '()
      (if (and (empty-tree? (left-tree t)) (empty-tree? (right-tree t)))
       '()
          (list (root-tree t) (remove-leaves (left-tree t))
          (remove-leaves (right-tree t)))))
  )

(define (map-tree f t)
  (if (empty-tree? t)
      '()
      (list (f (root-tree t)) (map-tree f (left-tree t))
          (map-tree f (right-tree t))))
  )

(define (fold-tree f nv t)
  (if (empty-tree? t)
      nv
      (list (f (root-tree t)) (fold-tree f nv (left-tree t))
          (fold-tree f nv (right-tree t))))
  )

(define (invert t)
  (if (empty-tree? t)
      '()
      (list (root-tree t) (invert (right-tree t)) (invert (left-tree t)))
  ))

(define (smaller x t)
  (if (empty-tree? t)
      #t
      (if (< x (root-tree t))
          #f
          (and (smaller x (left-tree t)) (smaller x (right-tree t))))
      )
  )

(define (bigger x t)
  (if (empty-tree? t)
      #t
      (if (> x (root-tree t))
          #f
          (and (bigger x (left-tree t)) (bigger x (right-tree t))))
      )
  )

(define (bst? t)
  (if (empty-tree? t)
      #t
      (if (not (and (smaller (root-tree t) (left-tree t))
                    (bigger (root-tree t) (right-tree t))))
          #f
          (and (bst? (left-tree t)) (bst? (right-tree t))))
      )
  )


;end

(define (memv-tree x t)
  (cond ((empty-tree? t) #f)
        ((eqv? x (root-tree t)) #t)
        (else (or (memv-tree x (left-tree t))
                  (memv-tree x (right-tree t))))))

(define (path-tree x t)
  (cond ((empty-tree? t) #f)
        ((eqv? x (root-tree t)) (list x))
        (else (cons#f (root-tree t) (or (path-tree x (left-tree t))
                                        (path-tree x (right-tree t)))))))

(define (cons#f h t) (and t (cons h t)))

(define (transformCount t)
  (if (empty-tree? t)
      '()
      (if (and (empty-tree? (left-tree t)) (empty-tree? (right-tree t)))
       '(1)
          (list (+ 1 (car (transformCount (left-tree t))) (car (transformCount (right-tree t))) )
                   (transformCount (left-tree t))
                   (transformCount (right-tree t)))))
  )

(define (transformSum t)
  (if (empty-tree? t)
      '()
      (if (and (empty-tree? (left-tree t)) (empty-tree? (right-tree t)))
       (list (root-tree t))
          (list (+ (car (transformSum (left-tree t))) (car (transformSum (right-tree t))) )
                   (transformSum (left-tree t))
                   (transformSum (right-tree t)))))
)

;asociativni spisaci
(define s1 '((1 . 2) (2 . 3) (3 . 4)))
(define (1+) (λ(x) (+ 1 x)))
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
(define g '((1 2 3) (2 3) (3 4 5) (4) (5 2 4 6) (6 2)))

(define (index l)
  (map (lambda (x) (cons (pos x l) x)) l)
  )
(define (pos x l)
  (if (= x (car l))
      1
      (+ 1 (pos x (cdr l))))
  )


(define (histogram l)
  (if (empty? l)
      l
  (cons (cons (car l) (br (car l) l) ) (histogram (del (car l) l)))
  ))
(define (br x l)
  (if (empty? l)
      0
      (if (= x (car l))
          (+ 1 (br x (cdr l)))
          (+ (br x (cdr l)))))
  )
(define (del x l)
  (if (empty? l)
      l
      (if (= x (car l))
          (flatten (del x (cdr l)))
          (flatten (list (car l) (del x (cdr l))))))
  )

(define (no-duplicate l)
  (if (empty? l)
      l
      (cons (car l) (no-duplicate (del-assoc (caar l) (cdr l)))))
  )

(define (merge l1 l2 op)
  (cond
    ((empty? l1) l2)
    ((empty? l2) l1)
    (else (dup (append l1 l2) op))
    )
  )
(define (dup l op)
    (if (empty? l)
      l
      (if (not (empty?(assoc (caar l) (cdr l))))
          (cons(cons (caar l)
                     (op (cdar l) (cdr (assoc (caar l) (cdr l)))))
               (dup (del-assoc (caar l)(cdr l)) op))
          (cons (car l) (dup (cdr l) op))
          ))
  )
(define s2 '((1 . 2) (2 . 3) (3 . 4)))
(define s3 '((2 . 20) (4 . 40) (6 . 60)))

(define (compose l1 l2)
  (if (empty? l1)
      l1
      (if (not (eq? (assoc (cdar l1) l2) #f))
          (cons (cons (caar l1) (cdr (assoc (cdar l1) l2)))
                (compose (cdr l1) l2))
          (compose (cdr l1) l2)))
  )
(define (make-rev-alist f keys)
  (map (lambda (x) (cons (f x) x )) keys))

(define (dup2 l op)
    (if (empty? l)
      l
      (if (not (empty?(assoc (caar l) (cdr l))))
          (cons(cons (caar l)
                     (op (cdar l)
                         (cdr (assoc (caar l) (cdr l)))
                         (cdr (assoc (caar l) (del1 (caar l)(cdr l))))))
               (dup2 (del-assoc (caar l)(cdr l)) op))
          (cons (car l) (dup2 (cdr l) op))
          )))

(define (del1 x l)
  (if (empty? l)
      l
      (if (= x (caar l))
          (cdr l)
          (cons (car l) (del1 x (cdr l)))))
  )
;only for 3 arguments
(define (group-by f l)
  (dup2 (make-rev-alist f l) (λ(x y z) (list x y z)))
  )
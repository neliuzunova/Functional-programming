#lang racket
;zad 1
(define (1+ n) (+ 1 n))
(define (length L)
  (if (null? L)
      0
      (1+ (length (cdr L)))))

(define (one . arg)
  (if (= (length arg) 0)
      1
      (if (= (length arg) 1)
          ( (car arg) 1)
          #f)))
(define (two . arg)
  (if (= (length arg) 0)
      2
      (if (= (length arg) 1)
          ( (car arg) 2)
          #f)))
(define (three . arg)
  (if (= (length arg) 0)
      3
      (if (= (length arg) 1)
          ( (car arg) 3)
          #f)))
(define (four . arg)
  (if (= (length arg) 0)
      4
      (if (= (length arg) 1)
          ( (car arg) 4)
          #f)))
(define (five . arg)
  (if (= (length arg) 0)
      5
      (if (= (length arg) 1)
          ( (car arg) 5)
          #f)))
(define (six . arg)
  (if (= (length arg) 0)
      6
      (if (= (length arg) 1)
          ( (car arg) 6)
          #f)))
(define (seven . arg)
  (if (= (length arg) 0)
      7
      (if (= (length arg) 1)
          ( (car arg) 7)
          #f)))
(define (eight . arg)
  (if (= (length arg) 0)
      8
      (if (= (length arg) 1)
          ( (car arg) 8)
          #f)))
(define (nine . arg)
  (if (= (length arg) 0)
      9
      (if (= (length arg) 1)
          ( (car arg) 9)
          #f)))

(define (plus x) (λ(y) (+ y x)))
(define (minus x) (λ(y) (- y x)))
(define (times x) (λ(y) (* y x)))
(define (div x) (λ(y) (/ y x)))

(require rackunit)
(require rackunit/text-ui)


(run-tests (test-suite "1zad"
                       (check-eq? (one (plus (three))) 4)
                       (check-eq? (three (times (five))) 15)
                       (check-eq? (nine (div (three))) 3)
                       (check-eq? (six (div (two))) 3)
                       (check-eq? (eight (minus (four))) 4)
                       (check-eq? (eight (minus (nine))) -1))
           'verbose)


;zad2
;(define s '(1 2 3))

(define head car)
(define (tail s) (force (cdr s))) 
(define empty-stream? null?)

(define (first n s)
  (if (or (empty-stream? s) (= n 0))
      '()
      (cons (head s) (first (- n 1) (tail s)))
      )
  )

(define (prefixes xs)
  (define (help k)
    (if (= k (length xs))
        (list xs)
        (cons (first k xs) (help (+ 1 k)))
        ))
  (help 0)
)

(run-tests (test-suite "2zad"
                       (check-equal? (prefixes '(1 2 3)) '(() (1) (1 2) (1 2 3)))
                       (check-equal? (prefixes '((10 20) (30))) '(() ((10 20)) ((10 20) (30))))
                       (check-equal? (prefixes '(() 7 (10))) '(() (()) (() 7) (() 7 (10)))))
           'verbose)

;zad 3

(define (make-date d m y)
  (cons d (cons m (cons y '() ))))

(run-tests (test-suite "3.0.zad"
                       (check-equal? (make-date 3 4 2000)   '(3 4 2000))
                       (check-equal? (make-date 03 4 2000)  '(3 4 2000))
                       (check-equal? (make-date 07 17 2700) '(7 17 2700))
                       (check-equal? (make-date 07 -17 -2700) '(7 -17 -2700)))
           'verbose)

(define L1 '(1 13 2019))

;1)
(define (day L)   (car L))
(define (month L) (cadr L))
(define (year L)  (caddr L))

(run-tests (test-suite "3.1.zad"
                       (check-eq? (day '(3 4 2000)) 3)
                       (check-eq? (month '(3 4 2000)) 4)
                       (check-eq? (year '(7 17 2700)) 2700)
                       (check-eq? (month '(7 -17 -2700)) -17))
           'verbose)

;2)
(define (isLeap? year)
  ( or (and (= (remainder year 4) 0)
            (not (= (remainder year 100) 0)))
       (= (remainder year 400) 0) )
  )

(define (date? L)
  (cond
    ((and (and (= (month L) 2) (isLeap? (year L))) (and (> (day L) 0) (< (day L) 30) )) #t)
    ((and (and (= (month L) 2) (not (isLeap? (year L)))) (and (> (day L) 0) (< (day L) 29) )) #t)
    ((and (or (= (month L) 1)(= (month L) 3)(= (month L) 5)(= (month L) 7)(= (month L) 8)(= (month L) 10)(= (month L) 12)) (and (> (day L) 0) (< (day L) 32) ))  #t)
    ((and (or (= (month L) 4)(= (month L) 6)(= (month L) 9)(= (month L) 11)) (and (> (day L) 0) (< (day L) 31) ))  #t)
    (else #f)
  )
)

(run-tests (test-suite "3.2.zad"
                       (check-eq? (date? (make-date 21 11 2019)) #t)
                       (check-eq? (date? (make-date 21 11 -2019)) #t)
                       (check-eq? (date? (make-date 51 11 2019)) #f)
                       (check-eq? (date? (make-date 21 13 2019)) #f)
                       (check-eq? (date? (make-date 29 2 2100)) #f)
                       (check-eq? (date? (make-date 29 2 2000)) #t))
           'verbose)

;3)
(define (date->string L)
  (string-append (number->string (day L)) "." (number->string (month L)) "." (number->string (year L)))
  )

(run-tests (test-suite "3.3.zad"
                       (check-equal? (date->string (make-date 21 11 2019)) "21.11.2019")
                       (check-equal? (date->string (make-date 1 2 -1239)) "1.2.-1239")
                       (check-equal? (date->string (make-date 06 05 -300)) "6.5.-300")
                       (check-equal? (date->string (make-date -06 -05 -300)) "-6.-5.-300"))
           'verbose)
;4)
(define (next-day L)
  (define N (append (list(+(day L) 1)) (cdr L)) )
  (if (date? N)
      N
      (if (< (month N) 12)
          (append (list 1) (list (+(month L)1) (year L)))
          (if (= (month N) 12)
                 (append (list 1) (list 1) (list (+ (year L) 1)) )
                 #f)
                 )
      )
  )

(run-tests (test-suite "3.4.zad"
                       (check-equal? (date->string (next-day (make-date 21 11 2019))) "22.11.2019")
                       (check-equal? (date->string (next-day (make-date 30 11 2019)))  "1.12.2019")
                       (check-equal? (date->string (next-day (make-date 31 12 2019)))  "1.1.2020")
                       (check-equal? (date->string (next-day (make-date 02 02 2020)))  "3.2.2020"))
           'verbose)

;5)
(define (date< L M)
  (cond
    ( (< (year L) (year M)) #t)
    ( (> (year L) (year M)) #f)
    ( (< (month L) (month M)) #t)
    ( (> (month L) (month M)) #f)
    ( (< (day L) (day M)) #t)
    ( (> (day L) (day M)) #f)
    (else #f)
    )
  )

(run-tests (test-suite "3.5.zad"
                       (check-eq? (date< (make-date 21 11 2019) (make-date 1 1 2020))  #t)
                       (check-eq? (date< (make-date 21 11 2019) (make-date 1 1 2019))  #f)
                       (check-eq? (date< (make-date 21 11 2100) (make-date 22 11 2100))  #t)
                       (check-eq? (date< (make-date 21 11 21000) (make-date 22 11 21000))  #t)
                       (check-eq? (date< (make-date 21 11 2019) (make-date 21 11 2019))  #f))
           'verbose)

;6)
(define (weekday L)
  (define JDN (floor(+(-(+(/(* 1461(+ (year L) 4800 (/(- (month L) 14)12))) 4) (/(* 367 (- (month L) 2 (* 12 (/(- (month L) 14)12)) )) 12))
                (/(* 3 (/(+ (year L) 4900 (/(- (month L) 14)12))100))4)
                32075)
                (day L)))
                )
  (define W1 (remainder (+ JDN 1) 7))
  (cond
    ( (= W1 0) 'Sunday)
    ( (= W1 1) 'Monday)
    ( (= W1 2) 'Tuesday)
    ( (= W1 3) 'Wednesday)
    ( (= W1 4) 'Thursday)
    ( (= W1 5) 'Friday)
    ( (= W1 6) 'Saturday)
    )
  )
; New Style Calendar works after 1582 year
(run-tests (test-suite "3.6.zad"
                       (check-equal? (weekday (make-date 21 11 2019)) 'Thursday)
                       (check-equal? (weekday (make-date 22 11 2019)) 'Friday)
                       (check-equal? (weekday (make-date 1 1 1600)) 'Saturday)
                       (check-equal? (weekday (make-date 4 12 2019)) 'Wednesday)
                       (check-equal? (weekday (make-date 6 12 2019)) 'Friday)
                       (check-equal? (weekday (make-date 8 12 2019)) 'Sunday))
           'verbose)

;7)
(define (next-weekday W D)
  (define (help D k)
    (if (and (eq? W (weekday D)) (not(= k 0)))
      D
      (help (next-day D) (+ 1 k)) )
    )
  (help D 0)
  )

(run-tests (test-suite "3.7.zad"
                       (check-equal? (date->string (next-weekday 'Thursday (make-date 21 11 2019))) "28.11.2019")
                       (check-equal? (date->string (next-weekday 'Tuesday (make-date 21 11 2019))) "26.11.2019")
                       (check-equal? (date->string (next-weekday 'Wednesday (make-date 21 11 2019))) "27.11.2019")
                       (check-equal? (date->string (next-weekday 'Friday (make-date 10 12 2019))) "13.12.2019"))
           'verbose)

;8)

(define (events-for-day D L)
  (if (null? L)
      '()
      (if (equal? D (caar L))
          (cons (car L) (events-for-day D (cdr L)))
          (events-for-day D (cdr L))))
  )

(run-tests (test-suite "3.8.zad"
                       (check-equal? (events-for-day (make-date 27 11 2019)
                (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                      (cons (make-date 27 11 2019) "Спират водата в Младост")
                      (cons (make-date 28 11 2019) "Спират водата в Лозенец")))
                                  '(((27 11 2019) . "Първа лекция за Хаскел") ((27 11 2019) . "Спират водата в Младост")))
                      (check-equal? (events-for-day (make-date 24 12 2019)
                (list (cons (make-date 24 12 2019) "Бъдни Вечер")
                      (cons (make-date 25 12 2019) "Коледа")
                      (cons (make-date 24 12 2019) "Празници")))
                                  '(((24 12 2019) . "Бъдни Вечер") ((24 12 2019) . "Празници")))
                       )
           'verbose)

;9)
(define (events-for-day2 D L)
  (if (null? L)
      '()
      (if (equal? D (caar L))
          (cons (cdar L) (events-for-day2 D (cdr L)))
          (events-for-day2 D (cdr L))))
  )

(define (del-assoc key alist)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist)
  )

;not sorted
(define (calendar2 L)
  (if (null? L)
      L
      (cons (append (list (caar L)) (events-for-day2 (caar L) L))
            (calendar2 (del-assoc (caar L) (cdr L))))
      )
  )

;sort
(define (minimum L)
  (if (null? (cdr L))
      (car L)
      (if (date< (caar L)  (car(minimum (cdr L)))) ;two lists
          (car L)
          (minimum (cdr L))
          ))
  )

(define (remove x L)
  (cond ((null? L) L)
        ((equal? x (car L)) (cdr L))
        (else (cons (car L) (remove x (cdr L)))))
  )

(define (selection-sort L)
  (if (null? L)
      L
      (cons (minimum L)(selection-sort (remove (minimum L) L ))))
  )

(define (calendar L) (selection-sort (calendar2 L)))

(run-tests (test-suite "3.9.zad"
                      (check-equal? (calendar (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                (cons (make-date 25 12 2019) "Коледа")
                (cons (make-date 27 11 2019) "Спират водата в Младост")
                (cons (make-date 23 3 2018) "Концерт на Лепа Брена")))
                                  '(((23 3 2018) "Концерт на Лепа Брена")
       ((27 11 2019) "Първа лекция за Хаскел" "Спират водата в Младост")
       ((25 12 2019) "Коледа"))
                      (check-equal? (calendar (list (cons (make-date 24 12 2019) "Бъдни Вечер")
                      (cons (make-date 25 12 2019) "Коледа")
                      (cons (make-date 24 12 2019) "Празници")))
                                  '(((24 12 2019) "Бъдни Вечер" "Празници") ((25 12 2019) "Коледа"))
                       )))
           'verbose)

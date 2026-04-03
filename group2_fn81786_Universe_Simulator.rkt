#lang racket
(define (is-between a b M)
  (and (>= M a) (< M b)))

;options for the W boson
(define (W-separation time)
  (let ([L (/(random 3) 3)])
    (cond 
      [(is-between 0   1/3 L)  (list (cons (- time 1)  "позитрон и неутрино"))]
      [(is-between 1/3 2/3 L)  (list (cons (- time 1)  "антимюон и неутрино"))]
      [else                    (list (cons (- time 1)  "антитау лептон и неутрино"))]
      ))
  )

;options for separation
(define (W-boson time)
  (define (Separation start)
    (if (is-between 0 50 (random 101))
        (W-separation (+ 1 start))
        (Separation (+ 1 start))
      ))
  (Separation time))

;options for the  top-cvark
(define (top-cvark-separation time)
  (let ([L (/(random 3) 10)])
    (cond 
      [(is-between 0   1/3 L)  (append (list (cons (- time 1)  "W бозон и долен кварк"))   (W-boson time))]
      [(is-between 1/3 2/3 L)  (append (list (cons (- time 1)  "W бозон и странен кварк")) (W-boson time))]
      [else                    (append (list (cons (- time 1)  "W бозон и дънен кварк"))   (W-boson time))]
      ))
  )      

;options for separation
(define (top-cvark time)
  (define (Separation start)
    (if (is-between 0 12.95 (/ (random 100001) 1000))
        (top-cvark-separation (+ 1 start))
        (Separation (+ 1 start))
      ))
  (Separation time))

;options for the  top-cvark
(define (top-anticvark-separation time)
  (let ([L (/(random 3) 10)])
    (cond 
      [(is-between 0   1/3 L)  (append (list (cons (- time 1)  "W бозон и долен антикварк")) (W-boson time))]
      [(is-between 1/3 2/3 L)  (append (list (cons (- time 1)  "W бозон и странен антикварк")) (W-boson time))]
      [else                    (append (list (cons (- time 1)  "W бозон и дънен антикварк")) (W-boson time))]
      ))
  )

;options for separation
(define (top-anticvark time)
  (define (Separation start)
    (if (is-between 0 12.95 (/ (random 100001) 1000))
        (top-anticvark-separation (+ 1 start))
        (Separation (+ 1 start))
      ))
  (Separation time))

;options for the Z boson
(define (Z-separation time) 
  (let ([L (/(random 1000) 10)])
    (cond 
      [(is-between 0    20.6 L)  (list (cons (- time 1)  "неутрино и антинеутрино")) ]
      [(is-between 20.6   24 L)  (list (cons (- time 1)  "електрон и позитрон")) ]
      [(is-between 24   27.4 L)  (list (cons (- time 1)  "мюон и антимюон")) ]
      [(is-between 27.4 30.8 L)  (list (cons (- time 1)  "тау лептон и антитау лептон")) ]
      [(is-between 30.8   46 L)  (list (cons (- time 1)  "долен кварк и долен антикварк")) ]
      [(is-between 46   61.2 L)  (list (cons (- time 1)  "странен кварк и странен антикварк")) ]
      [(is-between 61.2 76.4 L)  (list (cons (- time 1)  "дънен кварк и дънен антикварк"))]
      [(is-between 76.4 88.2 L)  (list (cons (- time 1)  "горен кварк и горен антикварк")) ]
      [else                      (list (cons (- time 1)  "чаровен кварк и чаровен антикварк")) ]
      ))
  )

;options for separation
(define (Z-boson time)
  (define (Separation start)
    (if (is-between 0 50 (random 101))
        (Z-separation (+ 1 start))
        (Separation (+ 1 start))
      ))
  (Separation time))

;options for the Higgs boson
(define (Higgs-separation time)   
  (let ([L (/ (random 1000001) 10000)])
    (cond 
      [(is-between 0      64.8 L)    (list (cons (- time 1)  "един дънен кварк и един дънен антикварк")) ]
      [(is-between 64.8   78.9 L)    (append (list (cons (- time 1)  "два W бозона")) (W-boson time) (W-boson time))]
      [(is-between 78.9   87.72 L)   (list (cons (- time 1)  "два глуона"))]
      [(is-between 87.72  94.76 L)   (list (cons (- time 1)  "един тау лептон и един антитау лептон"))]
      [(is-between 94.76  98.03 L)   (list (cons (- time 1)  "един чаровен кварк и един чаровен антикварк"))]
      [(is-between 98.03  99.62 L)   (append (list (cons (- time 1)  "два Z бозона")) (Z-boson time) (Z-boson time))]
      [(is-between 99.62  99.843 L)  (list (cons (- time 1)  "два фотона"))]
      [(is-between 99.843 99.954 L)  (append (list (cons (- time 1)  "един Z бозон и един фотон")) (Z-boson time))] 
      [(is-between 99.954 99.9784 L) (list (cons (- time 1)  "един мюон и един анти мюон"))]
      [else                          (append (list (cons (- time 1)  "един топ кварк и един топ антикварк")) (top-cvark time) (top-anticvark time))]
      ))
  )

;options for separation
(define (Higgs-boson)
  (define (Separation start)
    (if (is-between 0 0.0433 (/ (random 1000001) 10000))
        (Higgs-separation (+ 1 start))
        (Separation (+ 1 start))
      ))
  (cons (cons 1  "Един Хигс бозон") (Separation 2)))

;assoc lists
(define (takt D L)
  (if (null? L)
       '()
      (if (equal? D (caar L))
          (cons (cdar L) (takt D (cdr L)))
          (takt D (cdr L))))
  )

(define (del-assoc key alist)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist)
  )

(define (takt2 L)
  (if (null? L)
      L
      (cons (append (list (caar L)) (takt (caar L) L))
            (takt2 (del-assoc (caar L) (cdr L))))
      )
  )

(define (minimum L)
  (if (null? (cdr L))
      (car L)
      (if (<= (caar L)  (car(minimum (cdr L)))) ;two lists
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
;

;makes sorted list
(define (universe L)
  (selection-sort (takt2 L)))

(define (last L)
  (if (= (length L) 1)
      (car L)
      (last (cdr L))))

(define (repair-3rdrow L)
  (cond
    ((equal? (cadadr L) "два W бозона") "един W бозон")
    ((equal? (cadadr L) "два Z бозона") "един Z бозон")
    (else ""))
  )

(define (repair L)
  (let ([Third (list (caaddr L) (repair-3rdrow L) (car (cdaddr L)))] [Fourth (append (cadddr L) (cdaddr L))])
   (print2 (list (car L) (cadr L) Third Fourth)))
  )

(define (add-foton L)
  (begin (print2 L) (display " и един фотон"))
  )

;print funtions 
(define (deep-print L)
  (if (= (length L) 1)
      (display (car L))
      (begin (display (car L)) (display " ") (deep-print (cdr L)))
  ))

(define (print2 L) 
  (if (= (length L) 1)
      (deep-print (car L))
      (begin (deep-print (car L)) (display "\n") (print2 (cdr L)))
  ))

(define (print L)
  (if (and (= (length L) 4) (and (not (equal? (cadadr L) "един топ кварк и един топ антикварк"))
                                 (not (equal? (cadar L)  "един топ кварк и един топ антикварк"))))
      (repair L)
      (if (and (= (length L) 3)(equal? (cadadr L) "един Z бозон и един фотон"))
          (add-foton L)
          (print2 L))
  ))

;bonus visualize
(define (visualize L)
  (cond
    ((= (length L) 2)(begin (display "             Хигс бозон \n")
                            (display "              |      |  \n")
                            (cond
                              ((equal? (cadadr L) "два глуона") (display "       един глуон  един глуон \n"))
                              ((equal? (cadadr L) "два фотона") (display "       един фотон  един фотон \n"))
                               (else (display (cadadr L))))))
    ((= (length L) 3) (begin (display "             Хигс бозон \n")
                            (display "              |      |  \n")
                            (cond
                               ((equal? (cadadr L) "два W бозона") (display "       един W бозон  един W бозон \n")
                                                                  (display "         |     |       |     |    \n"))
                               ((equal? (cadadr L) "два Z бозона") (display "       един Z бозон  един Z бозон \n")
                                                                  (display "         |     |       |     |    \n"))
                               ((equal? (cadadr L) "един Z бозон и един фотон") (display "       един Z бозон  един фотон \n")
                                                                  (display "         |     |        \n")))
                            (cond
                              ((equal? (cadadr L) "един Z бозон и един фотон")  (begin (display "  ") (display (cadr (last L)))))
                               (else (begin (display (cadr(last L))) (display " ") (display (caddr(last L))))))
                            )
     )
    ((and (= (length L) 4) (not (equal? (cadadr L) "един топ кварк и един топ антикварк")))
          (begin            (display "             Хигс бозон \n")
                            (display "              |      |  \n")
                            (cond
                              ((equal? (cadadr L) "два W бозона") (display "       един W бозон  един W бозон \n")
                                                                  (display "         |     |       |     |    \n"))
                               (else                              (display "       един Z бозон  един Z бозон \n")
                                                                  (display "         |     |       |     |    \n")))
                            (begin (display (cadr(cadddr L))) (display " ") (display (car(cdaddr L))))
                            ))
    )
  )

(define (start)
  (let ([L (universe (Higgs-boson))])
  (begin (print L) (display "\n\n") (visualize L))))
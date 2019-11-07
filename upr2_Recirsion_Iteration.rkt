#lang racket

(define (fact n)
  (define (help n result)
    (if (= n 1)
        result
        (help (- n 1) (* result n))))
  (help n 1))

(define (fib n)
  (define (help a b n)
    (if (<= n 1)
        b
        (help b (+ a b) (- n 1))))
  (help 0 1 n)
  )

(define (sum a b)
  (define (help a b result)
  (if (> (ceiling a) (floor b))
      (ceiling result)
      (help (+ a 1) b (+ result (ceiling a))))
  )
  (help a b 0)
  )

(define (bool-to-num b)
  (if b 1 0))

(define (find d n)
  (define (help dd nn count)
    (if (<= nn 0)
        count
        (help dd (quotient nn 10) (+ count (bool-to-num (= (remainder nn 10) dd))))))
  (help d n 0))

(define (count-digits num)
  (define (help num counter)
    (if (= num 0)
         counter
        (help (quotient num 10) (+ counter 1))))
  (if (= num 0)
         1
         (help (abs num) 0)))
  
(define (reverse-num n)
  (define (help n new)
    (if (= n 0)
        new
    (help (quotient n 10) (+ (* new 10) (remainder n 10))
    )))
  (help n 0))

(define (palindrome1 n)
   (= n (reverse-num n)))

(define(palindrome2 n)
  (define (help n pos)
    (if (<= pos (quotient (count-digits n) 2))
        (if (= (remainder (quotient n (expt 10 (- (count-digits n) pos))) 10 )
               ( quotient (remainder n (expt 10 pos))(expt 10 (- pos 1)) ) )
            (help n (+ pos 1))
            #f)
        #t))
  (help n 1))

(define (prime? n)
  (define (help n del)
    (if (< del n)
        (if (= (remainder n del) 0)
            #f
            (help n (+ del 1)))
        #t))
  (help n 2))


(define (npow x n)
  (define (help x n result)
    (if (= n 0)
        result
    (help x (- n 1) (* result x))))
  (help x n 1))


(define (odd? num) (= (modulo num 2) 1))
(define (even? num) (not (odd? num)))


(define (fast-pow x n)
  (define (help x n result)
    (if (= n 0)
        result
        (if (even? n)
            ;(help (help x (/ n 2) result) 2 1)
            (help x (- n 1) (* result x)))))
  (help x n 1))


(define (bin-to-dec n)
  (define (help n pos result)
    (if (< pos (count-digits n))
        (help n (+ pos 1) (+ result (* (quotient (remainder n (expt 10 (+ pos 1))) (expt 10 pos)) (expt 2 pos) ) ))
        result))
  (help n 0 0))


(define (dec-to-bin n)
  (define (help n result pos)
    (if (>= n 1)
        (help (quotient n 2) (+ result (*( expt 10 pos ) (remainder n 2))) (+ pos 1))
        result)
    )
  (help n 0 0))


(define (subnum? d n)
  (define (help d n pos)
    (if (not (= n 0))
      
    (if (< pos (count-digits d))
        (if (= ( remainder(quotient d (expt 10 pos)) 10 ) (remainder n 10))
            (help d (quotient n 10) (+ pos 1))
            (help d (quotient n 10) 0))
        #t)
    
     (or (= pos (count-digits d)) (= d 0))
           ))
  (help d n 0))


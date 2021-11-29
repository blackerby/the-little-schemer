#lang racket

(require test-engine/racket-tests)

#;(check-expect (rember-f = 5 '(6 2 5 3)) '(6 2 3))
#;(check-expect (rember-f eq? 'jelly '(jelly beans are good)) '(beans are good))
#;(check-expect (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade and (cake)))
#;(define rember-f
  (lambda (test? a l)
    (cond
      [(empty? l) '()]
      [(test? a (first l)) (rest l)]
      [else (cons (first l) (rember-f test? a (rest l)))])))

(check-expect ((eq?-c 'salad) 'salad) #t)
(check-expect ((eq?-c 'salad) 'tuna) #f)
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(check-expect (eq?-salad 'salad) #t)
(check-expect (eq?-salad 'tuna) #f)
(define eq?-salad (eq?-c 'salad))

(check-expect ((rember-f eq?) 'tuna '(tuna salad is good)) '(salad is good))
(check-expect ((rember-f eq?) 'tuna '(shrimp salad and tuna salad)) '(shrimp salad and salad))
(check-expect ((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?)) '(equal? eqan? eqlist? eqpair?))
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        [(empty? l) '()]
        [(test? a (first l)) (rest l)]
        [else (cons (first l)
                    ((rember-f test?) a (rest l)))])))) ; notice the function call

(define rember-eq? (rember-f eq?))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        [(empty? l) '()]
        [(test? (first l) old)
         (cons new (cons old (rest l)))]
        [else (cons (first l)
                    ((insertL-f test?) new old (rest l)))]))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        [(empty? l) '()]
        [(test? (first l) old)
         (cons old (cons new (rest l)))]
        [else (cons (first l)
                    ((insertR-f test?) new old (rest l)))]))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        [(empty? l) '()]
        [(eq? (first l) old) (seq new old (rest l))]
        [else (cons (first l) ((insert-g seq) new old (rest l)))]))))

#;(define insertL (insert-g seqL))
(define insertL (insert-g (lambda (new old l)
                            (cons new (cons old l)))))
(define insertR (insert-g seqR))

#;(define subst
  (lambda (new old l)
    (cond
      [(empty? l) '()]
      [(eq? (first l) old)
       (cons new (rest l))]
      [else (cons (first l) (subst new old (rest l)))])))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))

(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l))) ; #f here represents no value, i.e., there is no "new"

(define seqrem (lambda (new old l) l))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define operator
  (lambda (aexp)
    (first aexp)))

(define 1st-sub-exp
  (lambda (aexp)
    (first (rest aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (first (rest (rest aexp)))))

(check-expect (value '13) 13)
(check-expect (value '(+ 1 3)) 4)
(check-expect (value '(+ 1 (↑ 3 4))) 82)
(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [else ((atom-to-function (operator nexp)) (value (1st-sub-exp nexp))
                                                (value (2nd-sub-exp nexp)))])))

(define atom-to-function
  (lambda (x)
    (cond
      [(eq? x '+) +]
      [(eq? x '*) *]
      [(eq? x '↑) expt])))

(define multirember
  (lambda (a lat)
    (cond
      [(empty? lat) '()]
      [(eq? (first lat) a) (multirember a (rest lat))]
      [else (cons (first lat) (multirember a (rest lat)))])))

(check-expect ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and))
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(empty? lat) '()]
        [(test? (first lat) a) ((multirember-f test?) a (rest lat))]
        [else (cons (first lat) ((multirember-f test?) a (rest lat)))]))))

(check-expect (multirember-eq? 'tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and))
(define multirember-eq? (multirember-f eq?))

(define eq?-tuna
    (eq?-c 'tuna))

(check-expect (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and))
(define multiremberT
  (lambda (test? lat)
    (cond
      [(empty? lat) '()]
      [(test? (first lat)) (multiremberT test? (rest lat))]
      [else (cons (first lat) (multiremberT test? (rest lat)))])))

#;(check-expect (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend) ...)
(check-expect (multirember&co 'tuna '() a-friend) #t)
(check-expect (multirember&co 'tuna '(tuna) a-friend) #f)
(define multirember&co
  (lambda (a lat col)
    (cond
      [(empty? lat)
       (col '() '())]
      [(eq? (first lat) a)
       (multirember&co a (rest lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (first lat) seen))))]
      [else (multirember&co a (rest lat)
                            (lambda (newlat seen)
                              (col (cons (first lat) newlat) seen)))])))

(define a-friend
  (lambda (x y)
    (empty? y)))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
              (cons 'tuna seen))))

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat) seen)))

(define last-friend
  (lambda (x y)
    (length x)))

(define multiinsertL
  (lambda (new old lat)
    (cond
      [(empty? lat) '()]
      [(eq? (first lat) old)
       (cons new (cons old (multiinsertL new old (rest lat))))]
      [else (cons (first lat) (multiinsertL new old (rest lat)))])))

(define multiinsertR
  (lambda (new old lat)
    (cond
      [(empty? lat) '()]
      [(eq? (first lat) old)
       (cons old (cons new (multiinsertR new old (rest lat))))]
      [else (cons (first lat) (multiinsertR new old (rest lat)))])))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      [(empty? lat) '()]
      [(eq? (first lat) oldL)
       (cons new (cons oldL (multiinsertLR oldL oldR (rest lat))))]
      [(eq? (first lat) oldR)
       (cons oldR (cons new (multiinsertLR oldL oldR (rest lat))))]
      [else (cons (first lat) (multiinsertLR oldL oldR (rest lat)))])))

(check-expect (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) (lambda (lat L R) lat))
              '(chips salty and salty fish or salty fish and chips salty))
(check-expect (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) (lambda (lat L R) L))
              2)
(check-expect (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) (lambda (lat L R) R))
              2)
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      [(empty? lat)
       (col '() 0 0)]
      [(eq? (first lat) oldL)
       (multiinsertLR&co new oldL oldR (rest lat)
                       (lambda (newlat L R) (col (cons new (cons oldL newlat)) (add1 L) R)))]
      [(eq? (first lat) oldR)
       (multiinsertLR&co new oldL oldR (rest lat)
                       (lambda (newlat L R) (col (cons oldR (cons new newlat)) L (add1 R))))]
      [else (multiinsertLR&co new oldL oldR (rest lat)
                       (lambda (newlat L R) (col (cons (first lat) newlat) L R)))])))

#;(define my-even?
  (lambda (n)
    (= (* (quotient n 2) 2) n)))

(check-expect (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)) '((2 8) 10 (() 6) 2))
(define evens-only*
  (lambda (l)
    (cond
      [(empty? l) '()]
      [(atom? (first l))
       (cond [(even? (first l))
              (cons (first l) (evens-only* (rest l)))]
             [else (evens-only* (rest l))])]
      [else (cons (evens-only* (first l))
                  (evens-only* (rest l)))])))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

(define evens-only*&co
  (lambda (l col)
    (cond
      [(empty? l)
       (col '() 1 0)]
      [(atom? (first l))
       (cond
         [(even? (first l))
          (evens-only*&co (rest l) (lambda (newl p s)
                                     (col (cons (first l) newl) (* (first l) p) s)))]
         [else
          (evens-only*&co (rest l) (lambda (newl p s)
                                     (col newl p (+ (first l) s))))])]
      [else (evens-only*&co (first l)
                            (lambda (fl fp fs)
                              (evens-only*&co (rest l)
                                              (lambda (rl rp rs)
                                                (col (cons fl rl)
                                                     (* fp rp)
                                                     (+ fs rs))))))])))

(test)

#|

The 10th commandment, rephrased:

Build functions to collect the value of more than one computation at a time.

|#


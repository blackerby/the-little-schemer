#lang racket

(require test-engine/racket-tests)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      [(null? l) #true]
      [(atom? (car l)) (lat? (cdr l))]
      [else #false])))

(check-expect (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
              '((coffee) ((tea)) (and (hick))))
(check-expect (rember* 'sauce '(((tomato sauce))
                                ((bean) sauce)
                                (and ((flying)) sauce)))
              '(((tomato))
                ((bean))
                (and ((flying)))))
(define rember*
  (lambda (a l)
    (cond
      [(empty? l) '()]
      [(atom? (first l))
       (cond
         [(eq? a (first l)) (rember* a (rest l))]
         [else (cons (first l) (rember* a (rest l)))])]
      [else (cons (rember* a (first l)) (rember* a (rest l)))])))

(check-expect (insertR* 'roast 'chuck '((how much (wood))
                                        could
                                        ((a (wood) chuck))
                                        (((chuck)))
                                        (if (a) ((wood chuck)))
                                        could chuck wood))
              '((how much (wood))
                could
                ((a (wood) chuck roast))
                (((chuck roast)))
                (if (a) ((wood chuck roast)))
                could chuck roast wood))
(define insertR*
  (lambda (new old l)
    (cond
      [(empty? l) '()]
      [(atom? (first l))
       (cond
         [(eq? old (first l))
          (cons old (cons new (insertR* new old (rest l))))]
         [else (cons (first l) (insertR* new old (rest l)))])]
      [else (cons (insertR* new old (first l))
                  (insertR* new old (rest l)))])))

(check-expect (occur* 'banana '((banana)
                                (split ((((banana ice)))
                                        (cream (banana))
                                        sherbet))
                                (banana)
                                (bread)
                                (banana brandy)))
              5)
(define occur*
  (lambda (a l)
    (cond
      [(empty? l) 0]
      [(atom? (first l))
       (cond
         [(eq? a (first l)) (add1 (occur* a (rest l)))]
         [else (occur* a (rest l))])]
      [else (+ (occur* a (first l)) (occur* a (rest l)))])))

(check-expect (subst* 'orange 'banana '((banana)
                                (split ((((banana ice)))
                                        (cream (banana))
                                        sherbet))
                                (banana)
                                (bread)
                                (banana brandy)))
              '((orange)
                (split ((((orange ice)))
                        (cream (orange))
                        sherbet))
                (orange)
                (bread)
                (orange brandy)))
(define subst*
  (lambda (new old l)
    (cond
      [(empty? l) '()]
      [(atom? (first l))
       (cond
         [(eq? old (first l))
          (cons new (subst* new old (rest l)))]
         [else (cons (first l) (subst* new old (rest l)))])]
      [else (cons (subst* new old (first l))
                  (subst* new old (rest l)))])))

(check-expect (insertL* 'pecker 'chuck '((how much (wood))
                                         could
                                         ((a (wood) chuck))
                                         (((chuck)))
                                         (if (a) ((wood chuck)))
                                         could chuck wood))
              '((how much (wood))
                could
                ((a (wood) pecker chuck))
                (((pecker chuck)))
                (if (a) ((wood pecker chuck)))
                could pecker chuck wood))
(define insertL*
  (lambda (new old l)
    (cond
      [(empty? l) '()]
      [(atom? (first l))
       (cond
         [(eq? (first l) old)
          (cons new (cons old (insertL* new old (rest l))))]
         [else (cons (first l) (insertL* new old (rest l)))])]
      [else (cons (insertL* new old (first l))
                  (insertL* new old (rest l)))])))

(check-expect (member* 'chips '((potato) (chips ((with) fish) (chips))))
              #true)
(define member*
  (lambda (a l)
    (cond
      [(empty? l) #false]
      [(atom? (first l))
       (cond
         [(eq? (first l) a) #true]
         [else (member* a (rest l))])]
      [else (or (member* a (first l))
                (member* a (rest l)))])))

(check-expect (leftmost '((potato) (chips ((with) fish) (chips))))
              'potato)
(check-expect (leftmost '(((hot) (tuna (and))) cheese))
              'hot)
(define leftmost
  (lambda (l)
    (cond
      [(atom? (first l)) (first l)]
      [else (leftmost (first l))])))

(define eqan?
  (lambda (a1 a2)
    (cond
      [(and (number? a1) (number? a2)) (= a1 a2)]
      [(or (number? a1) (number? a2)) #false] ; from book solution
      [else (eq? a1 a2)])))

(check-expect (eqlist? '(strawberry ice cream) '(strawberry ice cream)) #true)
(check-expect (eqlist? '(strawberry ice cream) '(strawberry cream ice)) #false)
(check-expect (eqlist? '(banana ((split))) '((banana) (split))) #false)
(check-expect (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))) #false)
(check-expect (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) #true)
#;(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (empty? l1) (empty? l2)) #true]
      [(or (empty? l1) (empty? l2)) #false]
      [(and (atom? (first l1)) (atom? (first l2))) (and (eqan? (first l1) (first l2))
                                                        (eqlist? (rest l1) (rest l2)))]
      [(or (atom? (first l1)) (atom? (first l2))) #false]
      [else (and (eqlist? (first l1) (first l2))
                 (eqlist? (rest l1) (rest l2)))])))

(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (empty? l1) (empty? l2)) #true]
      [(or (empty? l1) (empty? l2)) #false]
      [else (and (equal? (first l1) (first l2))
                 (eqlist? (rest l1) (rest l2)))])))

(check-expect (equal? 1 1) #true)
(check-expect (equal? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) #true)
(define equal?
  (lambda (s1 s2)
    (cond
      [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
      [(or (atom? s1) (atom? s2)) #false]
      [else (eqlist? s1 s2)])))

(define rember
  (lambda (s l)
    (cond
      [(empty? l) '()]
      [(equal? (first l) s) (rest l)]
      [else (cons (first l)
                  (rember s (rest l)))])))

(test)
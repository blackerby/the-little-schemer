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

(define member?
  (lambda (a lat)
    (cond
      [(empty? lat) #false]
      [(equal? a (first lat)) #true]
      [else (member? a (rest lat))])))

(check-expect (set? '(apple peaches apple plum)) #false)
(check-expect (set? '(apples peaches pears plums)) #true)
(check-expect (set? '()) #true)
; my original version
#;(define set?
  (lambda (lat)
    (cond
      [(empty? lat) #true]
      [else (and (not (member? (first lat) (rest lat)))
                 (set? (rest lat)))])))

; book version
(define set?
  (lambda (lat)
    (cond
      [(empty? lat) #true]
      [(member? (first lat) (rest lat)) #false]
      [else (set? (rest lat))])))

#;(check-expect (makeset '(apple peach pear peach plum apple lemon peach))
              '(pear plum apple lemon peach))
#;(define makeset
  (lambda (lat)
    (cond
      [(empty? lat) '()]
      [(member? (first lat) (rest lat)) (makeset (rest lat))]
      [else (cons (first lat) (makeset (rest lat)))])))

(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [else
       (cond
         [(equal? (first lat) a)
          (multirember a (rest lat))]
         [else
          (cons (first lat)
                (multirember a (rest lat)))])])))

(check-expect (makeset '(apple peach pear peach plum apple lemon peach))
              '(apple peach pear plum lemon))
(define makeset
  (lambda (lat)
    (cond
      [(empty? lat) '()]
      [(cons (first lat) (makeset (multirember (first lat) (rest lat))))]))) ; multirember does the equality check!


(check-expect (subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings)) #true)
(check-expect (subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish)) #false)
#;(define subset?
  (lambda (set1 set2)
    (andmap (lambda (a) (member? a set2)) set1)))

(define subset?
  (lambda (set1 set2)
    (cond
      [(empty? set1) #true]
      [else (and (member? (first set1) set2)
                 (subset? (rest set1) set2))])))

(define rember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(equal? (car lat) a) (cdr lat)]
      [else (cons (car lat)
                  (rember a (cdr lat)))])))

(check-expect (eqset? '(6 large chickens with wings) '(6 large chickens with wings)) #true)
; my naive version
#;(define eqset?
  (lambda (set1 set2)
    (cond
      [(and (empty? set1) (empty? set2)) #true]
      [else (and (member? (first set1) set2)
                 (eqset? (rest set1) (rember (first set1) set2)))])))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(check-expect (intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese)) #true)
#;(define intersect?
  (lambda (set1 set2)
    (ormap (lambda (a) (member? a set2)) set1)))

(define intersect?
  (lambda (set1 set2)
    (cond
      [(empty? set1) #false]
      [else (or (member? (first set1) set2)
                (intersect? (rest set1) set2))])))
; "inverse" of subset?

(check-expect (intersect '(stewed tomatoes and macaroni) '(macaroni and cheese)) '(and macaroni))
(define intersect
  (lambda (set1 set2)
    (cond
      [(empty? set1) '()]
      [(member? (first set1) set2)
       (cons (first set1) (intersect (rest set1) set2))] ; rember unnecessary: we traverse set1, not set2
      [else (intersect (rest set1) set2)])))

(check-expect (union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))
              '(stewed tomatoes casserole macaroni and cheese))
#;(define union
  (lambda (set1 set2)
    (cond
      [(empty? set1) set2]
      [(empty? set2) set1]
      [else (cons (first set1)
                  (union (rest set1) (rember (first set1) set2)))])))
; write a check-member-of for this version

(define union
  (lambda (set1 set2)
    (cond
      [(empty? set1) set2] ; works even if both are empty
      [(member? (first set1) set2) (union (rest set1) set2)] ; skips the one in set1 if it's in set2
      [else (cons (first set1) (union (rest set1) set2))])))

(check-expect (difference '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))
              '(stewed tomatoes casserole))
(define difference
  (lambda (set1 set2)
    (cond
      [(empty? set1) '()]
      [(member? (first set1) set2)
       (difference (rest set1) set2)]
      [else (cons (first set1)
                  (difference (rest set1) set2))])))

(check-expect (intersectall '((a b c) (c a d e) (e f g h a b))) '(a))
(check-expect (intersectall '((6 pears and)
                              (3 peaches and 6 peppers)
                              (8 pears and 6 plums)
                              (and 6 prunes with some apples)))
              '(6 and))
(define intersectall
  (lambda (l-set)
    (cond
      [(empty? (rest l-set)) (first l-set)] ; standard clause for nonempty list
      [else (intersect (first l-set)
                       (intersectall (rest l-set)))])))

(check-expect (a-pair? '(pear pear)) #true)
(check-expect (a-pair? '(3 7)) #true)
(check-expect (a-pair? '((2) (pair))) #true)
(check-expect (a-pair? '(full (house))) #true)
#;(define a-pair?
  (lambda (l)
      (and (or (atom? (first l))
               (lat? (first l)))
           (or (atom? (first (rest l)))
               (lat? (first (rest l))))
           (empty? (rest (rest l))))))

; book version
(define a-pair?
  (lambda (x)
    (cond
      [(atom? x) #false]
      [(empty? x) #false]
      [(empty? (rest x)) #false]
      [(empty? (rest (rest x))) #true]
      [else #false])))

;;; skipping the first, second, third functions, will use what's built into racket instead

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define firsts ; revision of my implementation in 03-cons-the-magnificent.rkt, more in line with book
  (lambda (l)
    (cond
      [(empty? l) '()]
      [else
       (cons (first (first l))
             (firsts (rest l)))])))

(check-expect (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))) #true)
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(check-expect (revrel '((a 8) (pie pumpkin) (sick got)))
              '((8 a) (pumpkin pie) (got sick)))
#;(define revrel
  (lambda (rel)
    (cond
      [(empty? rel) '()]
      [else (cons (build (second (first rel)) (first (first rel)))
                  (revrel (rest rel)))])))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      [(empty? rel) '()]
      [else (cons (revpair (first rel))
                  (revrel (rest rel)))])))

(check-expect (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4))) #false)
(check-expect (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4))) #true)
(check-expect (fullfun? '((grape raisin) (plum prune) (stewed prune))) #false)
(check-expect (fullfun? '((grape raisin) (plum prune) (stewed grape))) #true)
(define fullfun?
  (lambda (fun)
    (and (set? (firsts fun)) ; not in books implementation; written to ensure fun is actually fun!
         (set? (seconds fun)))))

(define seconds
  (lambda (l)
    (cond
      [(empty? l) '()]
      [else (cons (first (rest (first l)))
                  (seconds (rest l)))])))

(check-expect (one-to-one? '((8 3) (4 2) (7 6) (6 2) (3 4))) #false)
(check-expect (one-to-one? '((8 3) (4 8) (7 6) (6 2) (3 4))) #true)
(check-expect (one-to-one? '((grape raisin) (plum prune) (stewed prune))) #false)
(check-expect (one-to-one? '((grape raisin) (plum prune) (stewed grape))) #true)
(define one-to-one? ; copied from book. wow!
  (lambda (fun)
    (and (fun? fun) ; extra line to make more similar to fullfun?
         (fun? (revrel fun)))))
; book implementation assumes fun is a function

(test)

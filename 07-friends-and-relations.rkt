#lang racket

(require test-engine/racket-tests)

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

(test)

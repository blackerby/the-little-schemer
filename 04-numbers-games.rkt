;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 04-numbers-games) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (o+ 46 12) 58)
(define o+
  (lambda (x y)
    (cond
      [(zero? x) y]
      [else (add1 (o+ (sub1 x) y))])))

(check-expect (o- 46 12) 34)
(define o-
  (lambda (x y)
    (cond
      [(zero? y) x]
      [else (o- (sub1 x) (sub1 y))]))) ; is this tail call optimized?

(check-expect (book-o- 46 12) 34)
(define book-o-
  (lambda (x y)
    (cond
      [(zero? y) x]
      [else (sub1 (book-o- x (sub1 y)))])))

(check-expect (o* 2 4) 8)
(check-expect (o* 8 2) 16)
#;(define o*
  (lambda (x y)
    (cond
      [(= y 1) x]
      [else (+ x (o* x (sub1 y)))])))

(check-expect (addtup (list 15 6 7 12 3)) 43)
(define addtup
  (lambda (tup)
    (cond
      [(null? tup) 0]
      [else (o+ (first tup)
                (addtup (rest tup)))])))

(check-expect (o* 5 3) 15)
(define o*
  (lambda (n m)
    (cond
      [(zero? m) 0]
      [else (+ n (o* n (sub1 m)))])))

(check-expect (tup+ (list 3 6 9 11 4) (list 8 5 2 0 7)) (list 11 11 11 11 11))
(check-expect (tup+ (list 2 3) (list 4 6)) (list 6 9))
(check-expect (tup+ (list 3 7) (list 4 6 8 1)) (list 7 13 8 1))
(check-expect (tup+ (list 3 7 8 1) (list 4 6)) (list 7 13 8 1))
; assume tup1 and tup2 have the same length
#;(define tup+
  (lambda (tup1 tup2)
    (cond
      [(and (empty? tup1) (empty? tup2)) '()]
      [else (cons (+ (first tup1) (first tup2))
                  (tup+ (rest tup1) (rest tup2)))])))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ; [(and (empty? tup1) (empty? tup2)) '()] remove this to simplify
      [(empty? tup1) tup2]
      [(empty? tup2) tup1]
      [else (cons (+ (first tup1) (first tup2))
                  (tup+ (rest tup1) (rest tup2)))])))

(check-expect (o> 12 133) #false)
(check-expect (o> 120 11) #true)
(check-expect (o> 1 1) #false)
(check-expect (o> 12 12) #false)
(define o>
  (lambda (n m)
    (cond
      [(zero? n) #false]
      [(zero? m) #true]
      [else (o> (sub1 n) (sub1 m))])))

(check-expect (o< 12 133) #true)
(check-expect (o< 120 11) #false)
(check-expect (o< 1 1) #false)
(check-expect (o< 12 12) #false)
#;(define o<
  (lambda (n m)
    (cond
      [(and (zero? n) (> m 0)) #true]
      [(zero? m) #false]
      [else (o< (sub1 n) (sub1 m))])))

; book (better) solution
(define o<
  (lambda (n m)
    (cond
      [(zero? m) #false]
      [(zero? n) #true]
      [else (o< (sub1 n) (sub1 m))])))

(check-expect (o= 12 12) #true)
(check-expect (o= 12 13) #false)
(check-expect (o= 13 12) #false)
#;(define o=
  (lambda (n m)
    (cond
      [(zero? m) (zero? n)]
      [(zero? n) #false]
      [else (o= (sub1 n) (sub1 m))])))

(define o=
  (lambda (n m)
    (cond
      [(< n m) #false]
      [(> n m) #false]
      [else #true])))

(check-expect (↑ 1 1) 1)
(check-expect (↑ 2 3) 8)
(check-expect (↑ 5 3) 125)
(check-expect (↑ 11921312 0) 1)
(define ↑
  (lambda (n m)
    (cond
      [(zero? m) 1]
      [else (* n (↑ n (sub1 m)))])))

(check-expect (divide 16 2) 8)
(define divide
  (lambda (n m)
    (cond
      [(< n m) 0]
      [else (add1 (divide (- n m) m))])))

(check-expect (o-length (build-list 1000 identity)) 1000)
(define o-length
  (lambda (lat)
    (cond
      [(empty? lat) 0]
      [else (add1 (o-length (rest lat)))])))


(check-expect (pick 4 '(lasagna spaghetti ravioli macaroni meatball)) 'macaroni)
(define pick
  (lambda (n lat)
    (cond
      [(= 1 n) (first lat)] ; book version: [(zero? (sub1 n)) (cdr lat)]
      [else (pick (sub1 n) (rest lat))])))

(check-expect (rempick 3 '(hotdogs with hot mustard)) '(hotdogs with mustard))
#;(define rempick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (rest lat)]
      [else (cons (first lat) (rempick (sub1 n) (rest lat)))])))

;; rewritten for last exercise in chapter
(define rempick
  (lambda (n lat)
    (cond
      [(one? n) (rest lat)]
      [else (cons (first lat) (rempick (sub1 n) (rest lat)))])))


(check-expect (no-nums '(5 pears 6 prunes 9 dates)) '(pears prunes dates))
(define no-nums
  (lambda (lat)
    (cond
      [(empty? lat) '()]
      [(number? (first lat)) (no-nums (rest lat))]
      [else (cons (first lat) (no-nums (rest lat)))])))

(check-expect (all-nums '(5 pears 6 prunes 9 dates)) '(5 6 9))
(define all-nums
  (lambda (lat)
    (cond
      [(empty? lat) '()]
      [(number? (first lat)) (cons (first lat) (all-nums (rest lat)))]
      [else (all-nums (rest lat))])))

(check-expect (eqan? 1 1) #true)
(check-expect (eqan? 1 'a) #false)
(check-expect (eqan? 'a 'a) #true)
(define eqan?
  (lambda (a1 a2)
    (cond
      [(and (number? a1) (number? a2)) (= a1 a2)]
      [(or (number? a1) (number? a2)) #false] ; from book solution
      [else (eq? a1 a2)])))

(check-expect (occur 1 '(1 a 2 a 1 b)) 2)
(check-expect (occur 'a '(1 a 2 a 1 b)) 2)
(define occur
  (lambda (a lat)
    (cond
      [(empty? lat) 0]
      [(eqan? a (first lat)) (add1 (occur a (rest lat)))]
      [else (occur a (rest lat))])))

(check-expect (one? 1) #true)
(check-expect (one? 2) #false)
#;(define one?
  (lambda (n)
    (= n 1)))

;; OR

(define one?
  (lambda (n)
    (cond
      [(zero? n) #false]
      [else (zero? (sub1 n))])))

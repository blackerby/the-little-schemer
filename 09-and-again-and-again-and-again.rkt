#lang racket

(require test-engine/racket-tests)

(define pick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (first lat)]
      [else (pick (sub1 n) (rest lat))])))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define a-pair?
  (lambda (x)
    (cond
      [(atom? x) #false]
      [(empty? x) #false]
      [(empty? (rest x)) #false]
      [(empty? (rest (rest x))) #true]
      [else #false])))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

#;(define keep-looking
  (lambda (a sorn lat)
    (cond
      [(equal? a sorn) #t]
      [else (keep-looking a (pick sorn lat) lat)])))

; book version
(define keep-looking
  (lambda (a sorn lat)
    (cond
      [(number? sorn) (keep-looking a (pick sorn lat) lat)]
      [else (eq? sorn a)])))

; (define eternity (lambda (x) (eternity x)))

(check-expect (shift '((a b) c)) '(a (b c)))
(check-expect (shift '((a b) (c d))) '(a (b (c d))))
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (lambda (pora)
    (cond
      [(atom? pora) pora]
      [(a-pair? (first pora))
       (align (shift pora))]
      [else (build (first pora)
                   (align (second pora)))])))

(define length*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      ; [(empty? pora) 0] unnecessary
      [else (+ (length* (first pora))
               (length* (second pora)))])))

(define weight*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else (+ (* (weight* (first pora)) 2)
               (weight* (second pora)))])))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define shuffle
  (lambda (pora)
    (cond
      [(atom? pora) pora]
      [(a-pair? (first pora))
       (shuffle (revpair pora))]
      [else (build (first pora)
                   (shuffle (second pora)))])))

; uhhhh... gonna need to reread this one

(test)

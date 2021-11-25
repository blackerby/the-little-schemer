#lang racket

(define rember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) a) (cdr lat)]
      [else (cons (car lat)
                  (rember a (cdr lat)))])))

(define firsts ; not the book's implementation
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(cons? (car lat))
       (cons (car (car lat))
             (firsts (cdr lat)))]
       [else
        (cons (car lat)
              (firsts (cdr lat)))])))

(define insertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [else
       (cond
         [(eq? (first lat) old)
          (cons (first lat)
                (cons new (rest lat)))]
         [else
          (cons (first lat)
                (insertR new old (rest lat)))])])))

(define insertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [else
       (cond
         [(eq? (first lat) old)
          (cons new
                (cons (first lat) (rest lat)))]
         [else
          (cons (first lat)
                (insertL new old (rest lat)))])])))

(define subst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [else
       (cond
         [(eq? (first lat) old)
          (cons new (rest lat))]
         [else
          (cons (first lat)
                (subst new old (rest lat)))])])))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      [(null? lat) '()]
      [else
       (cond
         [(eq? (first lat) o1)
          (cons new (rest lat))]
         [(eq? (first lat) o2)
          (cons new (rest lat))]
         [else
          (cons (first lat)
                (subst2 new o1 o2 (rest lat)))])])))

(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [else
       (cond
         [(eq? (first lat) a)
          (multirember a (rest lat))]
         [else
          (cons (first lat)
                (multirember a (rest lat)))])])))

(define multiinsertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [else
       (cond
         [(eq? (first lat) old)
          (cons (first lat)
                (cons new (multiinsertR new old (rest lat))))]
         [else
          (cons (first lat)
                (multiinsertR new old (rest lat)))])])))

(define multiinsertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [else
       (cond
         [(eq? (first lat) old)
          (cons new (cons old (multiinsertL new old (rest lat))))]
         [else
          (cons (first lat) (multiinsertL new old (rest lat)))])])))

(define multisubst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [else
       (cond
         [(eq? (first lat) old)
          (cons new (multisubst new old (rest lat)))]
         [else
          (cons (first lat)
                (multisubst new old (rest lat)))])])))
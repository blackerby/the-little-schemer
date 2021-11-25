#lang racket

;; primitive
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; Chapter 2. Do It, Do It Again, and Again, and Again ...

(define lat?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(atom? (car l)) (lat? (cdr l))]
      [else #f])))

;; Atom ListOfAtom -> Boolean
;; return true if Atom in ListOfAtom
(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) a)
                (member? a (cdr lat)))])))

;; Chapter 3. Cons the Magnificent

;; Atom ListOfAtom -> ListOfAtom
;; remove Atom from ListOfAtom
#;
(define rember
  (lambda (a lat)
    (cond
      [(null? lat) empty]
      [(eq? (car lat) a) (cdr lat)]
      [else (cons (car lat)
                  (rember a (cdr lat)))])))

;; ListOfList -> ListOfAtom
;; return a list of the first element of each list in ListOfList
(define firsts
  (lambda (l)
    (cond
      [(null? l) empty]
      [else (cons (car (car l))
                  (firsts (cdr l)))])))

;; Atom Atom ListOfAtom -> ListOfAtom
;; insert first Atom to the right of first occurrence second Atom in ListOfAtom
(define insertR
  (lambda (new old lat)
    (cond
      [(null? lat) empty]
      [else
       (cond
         [(eq? (car lat) old)
          (cons (car lat) (cons new (cdr lat)))]
         [else
          (cons (car lat) (insertR new old (cdr lat)))])])))


;; Atom Atom ListOfAtom -> ListOfAtom
;; insert first Atom to the left of first occurrence of second Atom in ListOfAtom
(define insertL
  (lambda (new old lat)
    (cond
      [(null? lat) empty]
      [else
       (cond
         [(eq? (car lat) old)
          (cons new (cons (car lat) (cdr lat)))]
         [else
          (cons (car lat) (insertL new old (cdr lat)))])])))

;; Atom Atom ListOfAtom -> ListOfAtom
;; replace first occurrence of second Atom with first Atom in ListOfAtom
(define subst
  (lambda (new old lat)
    (cond
      [(null? lat) empty]
      [else
       (cond
         [(eq? (car lat) old)
          (cons new (cdr lat))]
         [else
          (cons (car lat) (subst new old (cdr lat)))])])))

;; Atom Atom Atom ListOfAtom -> ListOfAtom
;; replaces either the first occurrence of o1 or the first occurrence of o2 by new
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      [(null? lat) empty]
      [else
       (cond
         [(or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat))]
         [else
          (cons (car lat) (subst2 new o1 o2 (cdr lat)))])])))

;; Atom ListOfAtom -> ListOfAtom
;; produce lat with all instances of a removed
(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) empty]
      [else
       (cond
         [(eq? (car lat) a) (multirember a (cdr lat))]
         [else
          (cons (car lat) (multirember a (cdr lat)))])])))

;; Atom Atom ListOfAtom -> ListOfAtom
;; insert new to the right of each occurrence of old
(define multiinsertR
  (lambda (new old lat)
    (cond
      [(null? lat) empty]
      [else
       (cond
         [(eq? (car lat) old)
          (cons old (cons new (multiinsertR new old (cdr lat))))]
         [else
          (cons (car lat) (multiinsertR new old (cdr lat)))])])))

;; Atom Atom ListOfAtom -> ListOfAtom
;; insert new to the left of each occurrence of old
(define multiinsertL
  (lambda (new old lat)
    (cond
      [(null? lat) empty]
      [else
       (cond
         [(eq? (car lat) old)
          (cons new
                (cons old
                      (multiinsertL new old (cdr lat))))]
         [else
          (cons (car lat)
                (multiinsertL new old (cdr lat)))])])))

;; Atom Atom ListOfAtom -> ListOfAtom
;; substitute each occurrence of old with new
(define multisubst
  (lambda (new old lat)
    (cond
      [(null? lat) empty]
      [else
       (cond
         [(eq? (car lat) old)
          (cons new (multisubst new old (cdr lat)))]
         [else
          (cons (car lat) (multisubst new old (cdr lat)))])])))

;; Chapter 4. Numbers Games

;; Natural Natural -> Natural
;; produce the sum of n and m
(define o+
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else
       (add1 (o+ n (sub1 m)))])))

;; Natural Natural -> Natural
;; produce the difference of n and m
(define o-
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else
       (sub1 (o- n (sub1 m)))])))

;; Tup -> Natural
;; totals all the numbers in Tup
(define addtup
  (lambda (tup)
    (cond
      [(null? tup) 0]
      [else
       (o+ (car tup) (addtup (cdr tup)))])))

;; Natural Natural -> Natural
;; produces product of n and m
(define o*
  (lambda (n m)
    (cond
      [(zero? m) 0]
      [else
       (o+ n (o* n (sub1 m)))])))

;; Tup Tup -> Tup
;; given two tups of the same length, produces a new tup of the sums of
;; corresponding numbers in each tup
#;
(define tup+
  (lambda (tup1 tup2)
    (cond
      [(and (null? tup1) (null? tup2)) empty]
      [else
       (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))])))

;; Tup Tup -> Tup
;; given two tups of arbitrary length, produces new tup of the sums of
;; the corresponding numbers in each tup
(define tup+
  (lambda (tup1 tup2)
    (cond
      ;; [(and (null? tup1) (null? tup2)) empty] ; remove to simplify
      [(null? tup1) tup2]
      [(null? tup2) tup1]
      [else
       (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))])))

;; Number Number -> Boolean
;; return true if n is greater than m
(define o>
  (lambda (n m)
    (cond ;; order of conditions matters
      [(zero? n) #f]
      [(zero? m) #t]
      [else
       (o> (sub1 n) (sub1 m))])))

;; Number Number -> Boolean
;; return true if n is less than m
(define o<
  (lambda (n m)
    (cond ;; order of conditions still matters
      [(zero? m) #f]
      [(zero? n) #t]
      [else
       (o< (sub1 n) (sub1 m))])))

;; Number Number -> Boolean
;; return true if n and m are equal
#;
(define o=
  (lambda (n m)
    (cond
      [(zero? m) (zero? n)]
      [(zero? n) #f]
      [else
       (o= (sub1 n) (sub1 m))])))

(define o=
  (lambda (n m)
    (cond
      [(or (< n m) (> n m)) #f]
      [else #t])))

;; Number Number -> Number
;; raise n to the m power
(define ↑
  (lambda (n m)
    (cond
      ;; [(zero? n) 0] ; not necessary
      [(zero? m) 1]
      [else
       (* n (↑ n (sub1 m)))])))

;; Number Number -> Number
;; counts how many times m fits into n
(define ÷
  (lambda (n m)
    (cond
      [(< n m) 0]
      [else
       (add1 (÷ (- n m) m))])))

;; ListOfAtom -> Number
;; counts number of atoms in ListOfAtom
(define length
  (lambda (lat)
    (cond
      [(null? lat) 0]
      [else
       (add1 (length (cdr lat)))])))

;; Number ListOfAtom -> Atom
;; produces atom at index (starting from 1)
;; not my work -- had to look at solution
#;
(define pick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (car lat)]
      [else
       (pick (sub1 n) (cdr lat))])))

;; zero-based indexing
(define pick
  (lambda (n lat)
    (cond
      [(zero? n) (car lat)]
      [else
       (pick (sub1 n) (cdr lat))])))

;; Number ListOfAtom -> ListOfAtom
;; produces ListOfAtom with Atom at given 1-based index removed
#;
(define rempick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (cdr lat)]
      [else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))])))

;; ListOfAtom -> ListOfAtom
;; produce ListOfAtom with numbers removed
(define no-nums
  (lambda (lat)
    (cond
      [(null? lat) empty]
      [(number? (car lat))
       (no-nums (cdr lat))]
      [else
       (cons (car lat) (no-nums (cdr lat)))])))

;; ListOfAtom -> Tup
;; produces Tup from ListOfAtom
(define all-nums
  (lambda (lat)
    (cond
      [(null? lat) empty]
      [(number? (car lat))
       (cons (car lat) (all-nums (cdr lat)))]
      [else
       (all-nums (cdr lat))])))

;; Atom Atom -> Boolean
;; produce true if a1 and a2 are the same atom
(define eqan?
  (lambda (a1 a2)
    (cond
      [(and (number? a1) (number? a2)) (= a1 a2)]
      ;; book adds [(or (number? a1) (number? a2)) #f] ; necessary?
      [else (eq? a1 a2)])))

;; Atom ListOfAtom -> Number
;; counts number of times a appears in lat
(define occur
  (lambda (a lat)
    (cond
      [(null? lat) 0]
      [else
       (cond
         [(eq? (car lat) a) (add1 (occur a (cdr lat)))]
         [else
          (occur a (cdr lat))])])))

;; Number -> Boolean
;; returns true if number is 1
(define one?
  (lambda (n)
    (= n 1)))

;; Number ListOfAtom -> ListOfAtom
;; produces ListOfAtom with Atom at given 1-based index removed
(define rempick
  (lambda (n lat)
    (cond
      [(one? n) (cdr lat)]
      [else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))])))

;; Chapter 5. *Oh My Gawd*: It's Full of Stars

;; Atom List -> List
;; remove each occurrence of Atom from List
(define rember*
  (lambda (a l)
    (cond
      [(null? l) empty]
      [(atom? (car l))
       (cond
         [(eq? (car l) a)
          (rember* a (cdr l))]
         [else
          (cons (car l)
                (rember* a (cdr l)))])]
      [else
       (cons (rember* a (car l)) (rember* a (cdr l)))])))

;; Atom Atom List -> List
;; insert new to the right of each occurrence of old in list
(define insertR*
  (lambda (new old l)
    (cond
      [(null? l) empty]
      [(atom? (car l))
       (cond
         [(eq? (car l) old)
          (cons old (cons new (insertR* new old (cdr l))))]
         [else
          (cons (car l) (insertR* new old (cdr l)))])]
      [else
       (cons (insertR* new old (car l)) (insertR* new old (cdr l)))])))

;; Atom List -> Number
;; return number of occurrences of Atom in List
(define occur*
  (lambda (a l)
    (cond
      [(null? l) 0]
      [(atom? (car l))
       (cond
         [(eq? (car l) a)
          (add1 (occur* a (cdr l)))]
         [else
          (occur* a (cdr l))])]
      [else
       (+ (occur* a (car l)) (occur* a (cdr l)))])))

;; Atom Atom List -> List
;; replace each occurrence of old with new in list
(define subst*
  (lambda (new old l)
    (cond
      [(null? l) empty]
      [(atom? (car l))
       (cond
         [(eq? (car l) old)
          (cons new (subst* new old (cdr l)))]
         [else
          (cons (car l) (subst* new old (cdr l)))])]
      [else
       (cons (subst* new old (car l)) (subst* new old (cdr l)))])))

;; Atom Atom List -> List
;; insert new to the left of each occurrence of old
(define insertL*
  (lambda (new old l)
    (cond
      [(null? l) empty]
      [(atom? (car l))
       (cond
         [(eq? (car l) old) (cons new (cons old (insertL* new old (cdr l))))]
         [else
          (cons (car l) (insertL* new old (cdr l)))])]
      [else
       (cons (insertL* new old (car l)) (insertL* new old (cdr l)))])))

;; Atom List -> Boolean
;; produce true if atom anywhere in list
#;
(define member*
  (lambda (a l)
    (cond
      [(null? l) #f]
      [(atom? (car l))
       (cond
         [(eq? (car l) a) #t]
         [else
          (member* a (cdr l))])]
      [else
       (or (member* a (car l)) (member* a (cdr l)))])))

;; book's version
(define member*
  (lambda (a l)
    (cond
      [(null? l) #f]
      [(atom? (car l))
       (or (eq? (car l) a) (member* a (cdr l)))]
      [else
       (or (member* a (car l)) (member* a (cdr l)))])))

;; ListOfSexp -> Atom
;; returns the leftmost atom in l
(define leftmost
  (lambda (l)
    (cond
      [(atom? (car l)) (car l)]
      [else (leftmost (car l))])))

;; ListOfSexp ListOfSexp -> Boolean
;; return true if the two lists are equal
;; !!! had to look at solution
#;
(define eqlist?
  (lambda (l1 l2)
    (cond
      ; if both lists are null, the lists are equal
      [(and (null? l1) (null? l2)) #t]
      ; if l1 is null and l2 starts with an atom (is not null), the lists are not equal
      [(and (null? l1) (atom? (car l2))) #f]
      ; if l1 is null and l2 is not null but does NOT start with an atom, the lists are not equal
      [(null? l1) #f]
      ; if l1 is starts with an atom (is not null) and l2 is null, the lists are not equal
      [(and (atom? (car l1)) (null? l2)) #f]
      ; if each list starts with an atom
      [(and (atom? (car l1)) (atom? (car l2)))
       ; check if those atoms are equal and the rest of the list is equal
       (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]
      ; we get here if l2 is not null but does NOT start with an atom
      [(atom? (car l1)) #f]
      ; we get here if l1 is not null but l2 is
      [(null? l2) #f]
      ; we get here if neither l1 nor l2 is null and both start with lists
      ; not atoms
      [else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))])))

;; rewrite
#;
(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [else
       (cond
         [(and (atom? (car l1)) (atom? (car l2)))
          (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]
         [(atom? (car l1)) #f]
         [else
          (and (eqlist? (car l1) (car l2))
               (eqlist? (cdr l1) (cdr l2)))])])))

;; book's rewrite
#;
(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [(and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]
      [(or (atom? (car l1)) (atom? (car l2))) #f]
      [else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))])))

;; Sexp is one of:
;; - Atom
;; - ListOfSexp

;; Sexp Sexp -> Boolean
;; return true if s-expressions are equal
;; !!! had to look at solution
#;
(define equal?
  (lambda (s1 s2)
    (cond
      [(and (atom? s1) (atom? s2)) (eqan? s1 s2)] ; check for equality if both are atoms
      [(atom? s1) #f] ; first case failed, meaning s2 is a list
      [(atom? s2) #f] ; first and second case failed, meaning s1 is a list
      [else (eqlist? s1 s2)]))) ; s1 and s2 are lists, so compare as lists

;; rewrite
(define equal?
  (lambda (s1 s2)
    (cond
      [(and (atom? s1) (atom? s2)) (eqan? s1 s2)] ; check for equality if both are atoms
      [(or (atom? s1) (atom? s2)) #f] ; one of the s-exps is a list
      [else (eqlist? s1 s2)]))) ; s1 and s2 are lists, so compare as lists
          
(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [else
       (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))])))

;; Sexp ListOfSexp -> ListOfSexp
;; remove first matching Sexp in ListOfSexp
(define rember
  (lambda (s l)
    (cond
      [(null? l) empty]
      [(equal? (car l) s) (cdr l)]
      [else
       (cons (car l) (rember s (cdr l)))])))
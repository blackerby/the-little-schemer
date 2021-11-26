#lang racket

(require test-engine/racket-tests)

;; these are not line for line "solutions" for chapter 6 of The Little Schemer
;; but rather the result of working on the most interesting (to me)
;; parts of the code as I proceeded through the chapter. I have tried
;; to employ some lessons from How to Design Programs.

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; An Aexp (arithmetic expression) is one of:
; - Number
; - (list '+ Aexp Aexp)
; - (list '* Aexp Aexp)
; - (list '↑ Aexp Aexp)

(check-expect (numbered? '(3 + (4 ↑ 5))) #true)
(check-expect (numbered? '(2 * sausage)) #false)
(define numbered?
  (lambda (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [else (and (numbered? (first aexp))
                 (numbered? (first (rest (rest aexp)))))])))

(check-expect (value '13) 13)
(check-expect (value '(+ 1 3)) 4)
(check-expect (value '(+ 1 (↑ 3 4))) 82)
(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (operator nexp) '+)
       (+ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) '*)
       (* (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))]
      [else (expt (value (1st-sub-exp nexp))
                  (value (2nd-sub-exp nexp)))])))

(define 1st-sub-exp
  (lambda (aexp)
    (first (rest aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (first (rest (rest aexp)))))

(define operator
  (lambda (aexp)
    (first aexp)))

;;----------------------

(check-expect (sero? '()) #true)
(check-expect (sero? '(())) #false)
(define sero?
  (lambda (n)
    (empty? n)))

(check-expect (edd1 '()) '(()))
(define edd1
  (lambda (n)
    (cons '() n)))

(check-expect (zub1 '(() ())) '(()))
(define zub1
  (lambda (n)
    (rest n)))

(check-expect (o+ '(()) '(())) '(() ()))
(define o+
  (lambda (n m)
    (cond
      [(sero? m) n]
      [else (o+ (edd1 n) (zub1 m))]))) ; tail recursive?

(test)
;;; Author: Arthur Nunes-Harwitt

;;; data definitions

;; An arithmetic expression (ArithExp) is one of the following.
;; a number n
;; a variable x
;; a sum with parts e1 and e2, 
;;   where e1 and e2 are arithmetic expressions
;; a product with parts e1 and e2,
;;   where e1 and e2 are arithmetic expressions

;; functions for associated with each part: predicate, constructor, selectors.

;; number is a Scheme number

;; variable is a Scheme symbol
; variable?: Any -> Bool
(define variable? symbol?)
; variable=?: VarExp * VarExp -> Bool
(define variable=? eq?)


;; a sum is represented as a list with three elements: tag, e1, e2.
;; the tag is the symbol +
; sum?: Any -> Bool
(define (sum? a) (and (pair? a) (eq? (car a) '+)))
; make-sum: ArithExp * ArithExp -> SumExp
(define (make-sum e1 e2)
  (if (zero? e1)
   e2
   (if(zero? e2)
    e1
    (list '+ e1 e2))))



;; a product is represented as a list with three elements: tag, e1, e2.
;; the tag is the symbol *
; product?: Any -> Bool
(define (product? a) (and (pair? a) (eq? (car a) '*)))
; make-sum: ArithExp * ArithExp -> ProductExp
;(define (make-product e1 e2) (list '* e1 e2))

(define (make-product e1 e2)
  (if (zero? e1)
   '0
   (if(zero? e2)
    '0
    (if(= e1 1)
       e2
       (if (= e2 1)
           e1
           (list '* e1 e2))))))
;expt?: Any -> Bool
(define (expt? a) (and (pair? a) (eq? (car a) '^)))
(define (make-expt e n) 
  (if (zero? n)
      '1
      (if (= n 1)
          e
          (list '^ e n))))

;; sums and products will use the same selectors
; arg1: SumExp or ProdExp -> ArithExp
(define (arg1 e) (car (cdr e)))
; arg2: SumExp or ProdExp -> ArithExp
(define (arg2 e) (car (cdr (cdr e))))


;;; derivative code

;deriv: ArithExp * VarExp -> ArithExp
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (variable=? exp var) 1 0))
        ((sum? exp) 
         (make-sum (deriv (arg1 exp) var)
                   (deriv (arg2 exp) var)))
        ((product? exp)
         (make-sum (make-product (arg1 exp) (deriv (arg2 exp) var))
                   (make-product (arg2 exp) (deriv (arg1 exp) var))))
        ((expt? exp)
         (make-product (make-product (arg2 exp) (make-expt (arg1 exp) (- (arg2 exp) 1)))
                       (deriv (arg1 exp) var)))
                                     
        
        ; (^ arg1 arg2) --- (* (* arg2 (^ arg1 arg2-1)) (deriv (arg1 exp) var)                                              
                                                        
        (else (error 'deriv "Unexpected Input, not an ArithExp"))))



(define (zero-poly? poly)
  (if '() #t #f))
 
(define (poly<-const constant)
  (list constant) )
(define (poly<-var)
  (list 0 1))

(define (shift-left poly)
  (cons '0 poly))

(define (shift-right poly)
  (rev (cdr (rev poly))))
  
(define (rev lst)
  (if (null? lst)
      '()
      (append (rev (cdr lst))(list (car lst)))))


; (eq? (variable? 'x) #t)
; (eq? (variable? 3) #f)
; (variable=? 'x 'x)
; (not (variable=? 'x 'y))
; (eq? (sum? '(+ 2 3)) #t)
; (eq? (sum? '3) #f)
; (equal? (make-sum 2 3) '(+ 2 3))
; (eq? (product? '(* 2 3)) #t)
; (eq? (product? '3) #f)
; (equal? (make-product 2 3) '(* 2 3))
;(= (arg1 (make-sum 2 3)) 2)
;(= (arg1 (make-product 2 3)) 2)
;(= (arg2 (make-sum 2 3)) 3)
;(= (arg2 (make-product 2 3)) 3)
;(= (deriv 1 'x) 0)
;(= (deriv 'y 'x) 0)
;(= (deriv 'x 'x) 1)
;(deriv '(+ x x) 'x)
;(deriv '(^ x 2) 'x)
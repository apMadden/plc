;PROBLEMS STILL NOT FIXED:
;
;1) Doesn't handle one list having repeated values within itself. 
;
;2) Only returns the very first terminal. Does not repeat itself for multiple rules with the same left hand side value.
;2) Quotes still in output
;
;3) everything
;



; Terminals are quoted.
; A rule A -> X1 ... Xn is written (A (X1 ... Xn))
; A grammar is a list of rules.

(define *grammar*
  '((S (E 'eof))
    (E (T E2))
    (E2 ('+ T E2))
    (E2 ('- T E2))
    (E2 ())
    (T (F T2))
    (T2 ('* F T2))
    (T2 ('/ F T2))
    (T2 ())
    (F ('n))
    (F ('id))
    (F ('- F))
    (F ('OP E 'CP))))

; rule-lhs : Rule -> Variable
(define rule-lhs car)

; rule-rhs : Rule -> List(Variables or Terminals)
(define rule-rhs cadr)

; variable? : Any -> Boolean
(define variable? symbol?)

; terminal? : Any -> Boolean
(define (terminal? a) (and (pair? a) (eq? (car a) 'quote)))

; union : joins the two lists into one list with no repeated values.
(define (union a b)
  (cond ((null? a) b)
        ((null? b) a)
        ((member (car a) b) (union (cdr a) b))
        (else (cons (car a) (union (cdr a) b)))))


; find : takes a non-terminal or variable and returns a list of every rule in the grammar with that Variable on the LHS
(define (find rule)
    (get-rules rule *grammar*))
; get-rules : takes a non-terminal or variable and returns a list of every rule in the grammar with that Variable on the LHS
(define get-rules
   (lambda (var grammar)
      (define match (filter (lambda (item) (eq? var (rule-lhs item))) grammar))
      (if (null? match)
          '()
          match)))

; toRHS : takes all of the rules found from 'get-rules' and creates a new list with only the rules' right hand side(s).
(define (toRHS match)
  (if (null? match) '()
      (cons (rule-rhs(car match)) (toRHS (cdr match)))))



; first-alpha :
(define (first-alpha grammar alpha)
  (first3 grammar alpha '()))

; first3 :
(define (first3 grammar alpha seen)
  (cond ((null? alpha) '())                                                                                 ;epsilon
        ((terminal? (car alpha)) (list (car alpha)))                                                        ;terminal
        ((and (variable? (car alpha)) (member (car alpha) seen) (first3 grammar (cdr alpha) seen)))         ; if (car alpha) already exists in 'seen', skip to next value
        ((variable? (car alpha)) (first-var3 grammar (toRHS (find (car alpha))) (cons (car alpha) seen))))) ;variable that has not yet been seen 

                                 
;first-var3
(define (first-var3 grammar rules seen)
   (if(null? rules) '()                       ;if the rules are null, return an empty list
         (first3 grammar (car rules) seen)))    ; else call first3 on the first rule in the list of rules
        


;question 3


;get-follows: gets the every rule
(define get-follows
   (lambda (var grammar)
      (define match (filter (lambda (item) (memq var (rule-rhs item))) grammar))
      (if (null? match)
          '()
          match)))

(define (follow-var grammar var)
  (get-follows var grammar))

;(first-alpha *grammar* '(F))
(find 'F)
(toRHS (find 'F))
(first-alpha *grammar* '(E))
(follow-var *grammar* 'F)
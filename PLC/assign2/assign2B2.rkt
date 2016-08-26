;; Import the parser and lexer generators.
(require (lib "yacc.ss" "parser-tools")
         (lib "lex.ss" "parser-tools")
         (prefix : (lib "lex-sre.ss" "parser-tools")))

(require (lib "pretty.ss"))

(define-tokens value-tokens (NUM ID))

(define-empty-tokens op-tokens
  (OP
   CP
   COMMA
   EQ1
   LET
   IN   
   PROGRAM
   CLASS
   METHOD
   NEW
   SUPER
   SEQUENCE
   PROCEDURES
   IF
   PROC
   SEND
   FUNCALL
   ASSIGN
   EQUALITY   
   +
   -
   *
   /
   EOF))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ "A" "Z"))
  (letter (:or lower-letter upper-letter))
  (digit (:/ "0" "9"))
  ;(idfirst (:or letter (:or _ $)))    ; idfirst = letter | _ | $  
  (idrest (:or idfirst digit))         ;idrest = idfirst | digit  
  (digits (:+ digit))                  ;digits = digit+  
  ;(ident (:: idfirst (:* idrest)))    ;ident = idfirst idrest* 
  (ident (:+ letter))
  (number (:+ digit)))                 ;number = digits(.digits)?((E|e)(+|-)?digits)?
  

;get-token: inputPort -> token
(define get-token
  (lexer
   ((eof) 'EOF)
   ("let" 'LET)
   ("in" 'IN)
   ("(" 'OP)
   (")" 'CP)
   ("," 'COMMA)
   ("=" 'EQ1)
   ("+" '+)
   ("-" '-)
   ("*" '*)
   ("/" '/)   
   ("program" 'PROGRAM)
   ("class" 'CLASS)
   ("method" 'METHOD)
   ("new" 'NEW)
   ("super" 'SUPER)
   ("sequence" 'SEQUENCE)
   ("procedures" 'PROCEDURES)
   ("if" 'IF)
   ("proc" 'PROC)
   ("send" 'SEND)
   ("funcall" 'FUNCALL)
   ("assign!" 'ASSIGN)
   ("equality?" 'EQUALITY) 
   (number (token-NUM (string->number lexeme)))
   (ident (token-ID (string->symbol lexeme)))
   (whitespace (get-token input-port))))

;;; data definitions


;; A large language expression (LargeLangExp) is one of the following.
;; a number n
;; an identifier x

;; a sum with parts e1 and e2,
;; where e1 and e2 are Large language expressions

;; a difference with parts e1 and e2,
;; where e1 and e2 are Large language expressions

;; a product with parts e1 and e2,
;; where e1 and e2 are Large language expressions

;; a quotient with parts e1 and e2,
;; where e1 and e2 are Large language expressions

;; a negation with part e,
;; where e is an Large language expression

;; a bindings with parts defs and e,
;; where defs is a list of identifiers * LargeLangExp
;; and e is an Large language expression

;; a program with parts decls and expr,
;; where decls is a list of class declarations * LargeLangExp
;; and expr 

;; a class with parts name, parent, fields and methods
;; where name is a LargeLangExp representing the class name
;; parent is a LargeLangExp representing a parent class
;; fields is a list of field declarations * LargeLangExp
;; and methods is a list of method declarations * LargeLangExp

;; a method with parts name, formals, and exp
;; where name is a LargeLangExp representing the name of the method
;; formals is a list of formals * LargeLangExp
;; and exp is a LargeLangExp

;; a new with parts name and rands
;; where name is a LargeLangExp representing the name of the new
;; and rands is a list of LargeLangExps

;; a supercall with parts name and rands
;; where name is a LargeLangExp representing the name of the supercall
;; and rands is a list of LargeLangExps

;; a sequence with part exps
;; where exps is a list of LargeLangExps

;; procedures with parts defs and exp
;; where defs is a list of LargeLangExps
;; and exp is a LargeLangExp

;; an if with parts exp1 exp2 and exp3
;; where exp1 is a LargeLangExp
;; exp2 is a LargeLangExp
;; and exp3 is a LargeLangExp

;; an assign with parts var and exp
;; where var is a LargeLangExp
;; and exp is a LargeLangExp

;; an equality with parts exp1 and exp2
;; where exp1 is a LargeLangExp
;; and exp2 is a LargeLangExp

;; a proc with parts formals and exp
;; where formals is a list of LargeLangExp
;; and exp is a LargeLangExp

;; an access with parts exp and message
;; where exp is a LargeLangExp
;; and message is a LargeLangExp

;; a funcall with parts rator and rands
;; where rator is a LargeLangExp
;; and rands is a LargeLangExp


;; functions for associated with each part: predicate, constructor, selectors.

;; Number is a Scheme number
;; Identifier is a Scheme symbol

;; Method Structure

;; Class Structure

;; Program Structure


; Predicates

; sum?: Any -> Bool
(define (sum? a) (and (pair? a) (eq? (car a) 'sum)))

; difference?: Any -> Bool
(define (difference? a) (and (pair? a) (eq? (car a) 'diff)))

; product?: Any -> Bool
(define (product? a) (and (pair? a) (eq? (car a) 'prod)))

; quotient?: Any -> Bool
(define (quotient? a) (and (pair? a) (eq? (car a) 'quo)))

; negate?: Any -> Bool
(define (negate? a) (and (pair? a) (eq? (car a) 'neg)))

; let?: Any -> Bool
(define (let? a) (and (pair? a) (eq? (car a) 'with-bindings)))

; program?: Any -> Bool
(define (program? a) (and (pair? a) (eq? (car a) 'program)))

; class-decl?: Any -> Bool
(define (class-decl? a) (and (pair? a) (eq? (car a) 'class)))

; method?: Any -> Bool
(define (method? a) (and (pair? a) (eq? (car a) 'method)))

; new?: Any -> Bool
(define (new? a) (and (pair? a) (eq? (car a) 'new)))

; supercall?: Any -> Bool
(define (supercall? a) (and (pair? a) (eq? (car a) 'super)))

; seq?: Any -> Bool
(define (seq? a) (and (pair? a) (eq? (car a) 'sequence)))

; procs?: Any -> Bool
(define (procs? a) (and (pair? a) (eq? (car a) 'procedures)))

; if?: Any -> Bool
(define (if? a) (and (pair? a) (eq? (car a) 'if)))

; assign?: Any -> Bool
(define (assign? a) (and (pair? a) (eq? (car a) 'assign!)))

; equality?: Any -> Bool
(define (equality? a) (and (pair? a) (eq? (car a) 'equality?)))

; proc?: Any -> Bool
(define (proc? a) (and (pair? a) (eq? (car a) 'proc)))

; access?: Any -> Bool
(define (access? a) (and (pair? a) (eq? (car a) 'send)))

; funcall?: Any -> Bool
(define (funcall? a) (and (pair? a) (eq? (car a) 'funcall)))


;constructors

; make-sum: LargeLangExp * LargeLangExp -> SumExp
(define (make-sum exp1 exp2)
  (list 'sum exp1 exp2))

; make-diff: LargeLangExp * LargeLangExp -> DiffExp
(define (make-diff exp1 exp2)
  (list 'diff exp1 exp2))

; make-prod: LargeLangExp * LargeLangExp -> ProdExp
(define (make-prod exp1 exp2)
  (list 'prod exp1 exp2))

; make-quo: LargeLangExp * LargeLangExp -> QuoExp
(define (make-quo exp1 exp2)
  (list 'quo exp1 exp2))

; make-neg: LargeLangExp -> NegExp
(define (make-neg exp)
  (list 'neg exp))

; make-let: Listof(Identifier*LargeLangExp) * LargeLangExp -> BindingExp
; Identifier*LargeLangExp is represented as a two element list
(define (make-let defs exp)
  (list 'with-bindings defs exp))

; make-neg: LargeLangExp -> NegExp
(define (make-program exp1 exp2)
  (list 'program exp1 exp2))

; make-neg: LargeLangExp -> NegExp
(define (make-class exp1 exp2 exp3 exp4)
  (list 'class exp1 exp2 exp3 exp4))

; make-neg: LargeLangExp -> NegExp -------- formals
(define (make-method exp1 exp2)
  ;(cons method(cons arg1 arg2))
  (list 'method exp1 exp2))

; make-neg: LargeLangExp -> NegExp ;--------- actuals
(define (make-new exp1 exp2) 
  (list 'new exp1 exp2))

;                                      --------- actuals
(define (make-supercall exp1 exp2)
  (list 'super exp1 exp2))

(define (make-seq exp1)
  (list 'sequence exp1))

(define (make-procs exp1 exp2)
  (list 'procedures exp1 exp2))

(define (make-if exp1 exp2 exp3)
  (list 'if exp1 exp2 exp3))

(define (make-assign exp1 exp2)
  (list 'assign! exp1 exp2))

(define (make-equal exp1 exp2)
  (list 'equality? exp1 exp2))
                                   ; ------- formals
(define (make-proc exp1 exp2)
  (list 'proc exp1 exp2))

(define (make-access exp1 exp2)
  (list 'send exp1 exp2))
                                   ; --------- actuals
(define (make-funcall exp1 exp2)
  (list 'funcall exp1 exp2))

;; Selectors

; arg1:SmallLangExp -> SmallLangExp
(define (arg1 e) (car (cdr e)))

; arg2: SmallLangExp -> SmallLangExp
(define (arg2 e) (car (cdr (cdr e))))

; neg-exp: NegExp -> SmallLangExp
(define (neg-exp e) (car (cdr e)))

; let-defs: LetExp -> SmallLangExp
(define (let-defs e) (car (cdr e)))

; let-exp: LetExp -> SmallLangExp
(define (let-exp e) (car (cdr (cdr e))))

; program-decls: ProgExp -> SmallLangExp
(define (program-decls e) (car (cdr e)))

; program-exp: ProgExp -> SmallLangExp
(define (program-exp e) (car (cdr (cdr e))))

; let-exp: LetExp -> SmallLangExp
(define (class-name e) (car (cdr e)))

; let-exp: LetExp -> SmallLangExp
(define (class-parent e) (car (cdr (cdr  e))))

; let-exp: LetExp -> SmallLangExp
(define (class-fields e) (car (cdr (cdr (cdr  e)))))

; let-exp: LetExp -> SmallLangExp
(define (class-methods e) (car (cdr (cdr (cdr (cdr e))))))

; let-exp: LetExp -> SmallLangExp
(define (method-name e) (car (cdr e)))        ; -------- formals

; let-exp: LetExp -> SmallLangExp
(define (method-formals e) (car (cdr (cdr e)))) ;  -------- formals

; let-exp: LetExp -> SmallLangExp
(define (method-exp e) (car (cdr (cdr (cdr e))))) ;  -------- formals

; let-exp: LetExp -> SmallLangExp
(define (new-name e) (car (cdr e)))          ;--------- actuals

; let-exp: LetExp -> SmallLangExp
(define (new-rands e) (car (cdr e)))         ;--------- actuals

; let-exp: LetExp -> SmallLangExp
(define (supercall-name e) (car (cdr (cdr e)))) ;--------- actuals

; let-exp: LetExp -> SmallLangExp
(define (supercall-rands e) (car (cdr e)))        ; --------- actuals

; let-exp: LetExp -> SmallLangExp
(define (seq-exps e) (car (cdr e)))

; let-exp: LetExp -> SmallLangExp
(define (procs-defs e) (car (cdr e)))

; let-exp: LetExp -> SmallLangExp
(define (procs-exp e) (car (cdr (cdr e))))

; let-exp: LetExp -> SmallLangExp
(define (if-exp1 e) (car (cdr e)))

; let-exp: LetExp -> SmallLangExp
(define (if-exp2 e) (car (cdr (cdr e))))

; let-exp: LetExp -> SmallLangExp
(define (if-exp3 e) (car (cdr (cdr (cdr e)))))

; let-exp: LetExp -> SmallLangExp
(define (assign-var e) (car (cdr e)))

; let-exp: LetExp -> SmallLangExp
(define (assign-exp e) (car (cdr (cdr e))))

; let-exp: LetExp -> SmallLangExp
(define (proc-formals e) (car (cdr e)))    ; ------- formals

; let-exp: LetExp -> SmallLangExp
(define (proc-exp e) (car (cdr e)))        ; ------- formals

; let-exp: LetExp -> SmallLangExp
(define (access-exp e) (car (cdr (cdr e))))

; let-exp: LetExp -> SmallLangExp
(define (access-message e) (car (cdr e)))

; let-exp: LetExp -> SmallLangExp
(define (funcall-rator e) (car (cdr e))) ; --------- actuals

; let-exp: LetExp -> SmallLangExp
(define (funcall-rands e) (car (cdr (cdr e)))) ; --------- actuals

(define parse-small-lang
  (parser
   (start exp)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (error 'parse-small-lang "error occurred, ~v ~v ~v" a b c)))
   (grammar
    (exp ((LET let-defs IN exp) (make-let $2 $4))
         ((math-exp) $1))
    (let-def ((ID EQ1 exp) (list $1 $3)))
    (let-defs ((let-def) (list $1))
              ((let-def COMMA let-defs) (cons $1 $3)))
    (math-exp ((math-exp + term) (make-sum $1 $3))
              ((math-exp - term) (make-diff $1 $3))
              ((term) $1))
    (term ((term * factor) (make-prod $1 $3))
          ((term / factor) (make-quo $1 $3))
          ((factor) $1))
    (factor ((ID) $1)
            ((NUM) $1)
            ((- factor) (make-neg $2))
            ((OP exp CP) $2)))))
; lexer/parser test
; lexer/parser test
(let* ((example "let x = -2 + (3 * 4), y = 0 in -2+5*x+y")
       (i (open-input-string example))) ; convert string to inputPort
  (equal? (parse-small-lang (lambda () (get-token i)))
          '(with-bindings ((x (sum (neg 2) (prod 3 4)))
                           (y 0))
             (sum (sum (neg 2) (prod 5 x)) y))))

;(let* ((example "let x = -(1+1) + 3 * 4, y = 0 in {y = 14; x == y}")
;       (i (open-input-string example)))
;  (equal? (parse-lang (lambda () (get-token i)))
;          '(program
;            ()
;            (with-bindings
;             ((x (sum (neg (sum 1 1)) (prod 3 4))) (y 0))
;             (sequence (assign! y 14) (equality? x y))))))

;(let* ((example 
;"let pred = \\k\\->k-1 
;  in procedures f(n) = if n == 0
;                       then 1 
;                       else n * f(pred(n)) 
;      in f(4+1)
;")
;       (i (open-input-string example)))
;  (equal? (parse-lang (lambda () (get-token i)))
;          '(program
;            ()
;            (with-bindings
;             ((pred (proc (k) (diff k 1))))
;             (procedures
;              ((f
;                (proc
;                 (n)
;                 (if (equality? n 0) 1 (prod n (funcall f (funcall pred n)))))))
;              (funcall f (sum 4 1)))))))


;(let* ((example 
;"class point extends object{
;  field x
;  field y
;  method init(initx, inity){
;   x = initx;
;   y = inity
;  }
;  method move(dx, dy){
;   x = x + dx;
;   y = y + dy
;  }
;}
;let ob = new point(2+3, 1+4*7) in
;  ob.move(0.1,3)
;")
;       (i (open-input-string example)))
;  (equal? (parse-lang (lambda () (get-token i)))
;          '(program
;            ((class point object
;                    (x y)
;                    ((method
;                      init
;                      (initx inity)
;                      (sequence (assign! x initx) (assign! y inity)))
;                     (method
;                      move
;                      (dx dy)
;                      (sequence (assign! x (sum x dx)) (assign! y (sum y dy)))))))
;            (with-bindings
;             ((ob (new point (sum 2 3) (sum 1 (prod 4 7)))))
;             (funcall (send ob move) 0.1 3)))))


;;Constructor tests
;(equal? (make-sum 2 3) '(sum 2 3))
;(equal? (make-diff 2 3) '(diff 2 3))
;(equal? (make-prod 2 3) '(prod 2 3))
;(equal? (make-quo 2 3) '(quo 2 3))
;(equal? (make-neg 2) '(neg 2))
;(equal? (make-let (list (list 'x 1) (list 'y 2)) 3) '(with-bindings ((x 1) (y 2)) 3))

;; Predicate tests
;(eq? (sum? '(sum 2 3)) #t)
;(eq? (sum? '3) #f)
;(eq? (difference? '(diff 2 3)) #t)
;(eq? (sum? '3) #f)
;(eq? (product? '(prod 2 3)) #t)
;(eq? (product? '3) #f)
;(eq? (quotient? '(quo 2 3)) #t)
;(eq? (quotient? '3) #f)
;(eq? (negate? '(neg 2 3)) #t)
;(eq? (negate? '3) #f)
;(eq? (let? '(with-bindings 2 3)) #t)
;(eq? (let? '3) #f)

;; Selector tests                   -- The three tests labeled false expect a false result.
;(= (arg1 (make-sum 2 3)) 2)
;(= (arg1 (make-prod 2 3)) 2)
;(= (arg2 (make-sum 2 3)) 3)
;(= (arg2 (make-prod 2 3)) 3)
;(= (neg-exp (make-neg 2)) 2)
;(equal? (neg-exp (make-neg 2)) 'neg)                                                 ;false
;(equal? (let-defs (make-let (list (list 'x 1) (list 'y 2)) 3)) '((x 1) (y 2)))
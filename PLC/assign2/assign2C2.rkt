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
   +
   -
   *
   /
   OCB
   CCB
   IF
   THEN
   ELSE
   PROC
   SEND
   FUNCALL
   ASSIGN
   EQUALITY 
   BS
   ARROW
   NEW
   SUPER
   SEMICOLON
   EQ2
   DECIMAL
   CLASS
   EXTENDS
   FIELD
   METHOD
   PROCEDURES
   NEW
   SUPER   
   EOF))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ "A" "Z"))
  (letter (:or lower-letter upper-letter))
  (digit (:/ "0" "9"))
  (idfirst (:or letter (:or "_" "$")))
  (idrest (:or idfirst digit))
  (digits (:+ digit))
  (ident (:+ letter))  
  (number (:: (:: digits (:? (:: "." digits))) (:? (:: (:: (:or "E" "e") (:? (:or "+" "-"))) digits)))))


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
   ("{" 'OCB)
   ("}" 'CCB)
   ("if" 'IF)
   ("then" 'THEN)
   ("else" 'ELSE)
   ("proc" 'PROC)
   ("send" 'SEND)
   ("funcall" 'FUNCALL)
   ("assign!" 'ASSIGN)
   ("equality?" 'EQUALITY) 
   ("\\" 'BS)
   ("->" 'ARROW)
   ("new" 'NEW)
   ("super" 'SUPER)
   (";" 'SEMICOLON)
   ("==" 'EQ2)
   ("." 'DECIMAL)
   ("class" 'CLASS)
   ("extends" 'EXTENDS)
   ("field" 'FIELD)
   ("method" 'METHOD)
   ("procedures" 'PROCEDURES)
   ("new" 'NEW)
   ("super" 'SUPER)   
   (number (token-NUM (string->number lexeme)))
   (ident (token-ID (string->symbol lexeme)))
   (whitespace (get-token input-port))))

;;; data definitions


;; A small language expression (SmallLangExp) is one of the following.
;; a number n
;; an identifier x
;; a sum with parts e1 and e2,
;; where e1 and e2 are small language expressions
;; a difference with parts e1 and e2,
;; where e1 and e2 are small language expressions
;; a product with parts e1 and e2,
;; where e1 and e2 are small language expressions
;; a quotient with parts e1 and e2,
;; where e1 and e2 are small language expressions
;; a negation with part e,
;; where e is an small language expression
;; a bindings with parts defs and e,
;; where defs is a list of identifiers * SmallLangExp
;; and e is an small language expression
;; functions for associated with each part: predicate, constructor, selectors.
;; Number is a Scheme number
;; Identifier is a Scheme symbol


; Predicates

; sum?: Any -> Bool
(define (sum? a) (and (pair? a) (equal? (car a) 'sum)))

; difference?: Any -> Bool
(define (difference? a) (and (pair? a) (equal? (car a) 'diff)))

; product?: Any -> Bool
(define (product? a) (and (pair? a) (equal? (car a) 'prod)))

; quotient?: Any -> Bool
(define (quotient? a) (and (pair? a) (equal? (car a) 'quo)))

; negate?: Any -> Bool
(define (negate? a) (and (pair? a) (equal? (car a) 'neg)))

; let?: Any -> Bool
(define (let? a) (and (pair? a) (equal? (car a) 'with-bindings)))

; program?: Any -> Bool
(define (program? a) (and (pair? a) (equal? (car a) 'program)))

; class-decl?: Any -> Bool
(define (class-decl? a) (and (pair? a) (equal? (car a) 'class)))

; method?: Any -> Bool
(define (method? a) (and (pair? a) (equal? (car a) 'method)))

; new?: Any -> Bool
(define (new? a) (and (pair? a) (equal? (car a) 'new)))

; supercall?: Any -> Bool
(define (supercall? a) (and (pair? a) (equal? (car a) 'super)))

; seq?: Any -> Bool
(define (seq? a) (and (pair? a) (equal? (car a) 'sequence)))

; procs?: Any -> Bool
(define (procs? a) (and (pair? a) (equal? (car a) 'procedures)))

; if?: Any -> Bool
(define (if? a) (and (pair? a) (equal? (car a) 'if)))

; assign?: Any -> Bool
(define (assign? a) (and (pair? a) (equal? (car a) 'assign!)))

; equality?: Any -> Bool
(define (equality? a) (and (pair? a) (equal? (car a) 'equality?)))

; proc?: Any -> Bool
(define (proc? a) (and (pair? a) (equal? (car a) 'proc)))

; access?: Any -> Bool
(define (access? a) (and (pair? a) (equal? (car a) 'send)))

; funcall?: Any -> Bool
(define (funcall? a) (and (pair? a) (equal? (car a) 'funcall)))


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
  (list 'method exp1 exp2))

; make-neg: LargeLangExp -> NegExp ;--------- actuals
(define (make-new exp1 exp2)
   (cons 'new (cons arg1 arg2)))

;                                      --------- actuals
(define (make-supercall exp1 exp2)
    (cons 'super (cons arg1 arg2)))

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
    (cons 'funcall (cons arg1 arg2)))

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

(define parse-lang
  (parser
   (start program)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (error 'parse-lang "error occurred, ~v ~v ~v" a b c)))
   (grammar
    (program ((class-decls exp) (make-program $1 $2)))
    (class-decl ((CLASS ID EXTENDS ID OCB field-decls meth-decls CCB) (make-class $2 $4 $6 $7)))
    (class-decls (() null)
                 ((class-decl class-decls) (cons $1 $2)))
    (field-decls (() null)
                 ((FIELD ID field-decls) (cons $2 $3)))
    (meth-decls (() null)
                ((METHOD ID OP formals CP exp meth-decls) (cons (make-method $2 $4 $6) $7)))
    (exp ((LET let-defs IN exp) (make-let $2 $4))
         ((PROCEDURES proc-defs IN exp) (make-procs $2 $3))
         ((OCB exps CCB) (make-seq $2))
         ((IF exp THEN exp ELSE exp) (make-if $2 $4 $6))
         ((BS formals BS ARROW exp) (make-proc $2 $5))
         ((NEW ID OP actuals CP) (make-new $2 $4))
         ((SUPER ID OP actuals CP) (make-super $2 $4))
         ((ID EQ1 exp) (make-assign $1 $3))
         ((comp-exp) $1))
    (let-def ((ID EQ1 exp) (list $1 $3)))
    (let-defs ((let-def) (list $1))
              ((let-def COMMA let-defs) (cons $1 $3)))
    (proc-def ((ID OP formals CP EQ1 exp) (list $1 (make-proc $3 $6))))
    (proc-defs ((proc-def) (list $1))
               ((proc-def COMMA proc-defs) (cons $1 $3)))
    (exps ((exp) (list $1))
          ((exp SEMICOLON exps) (cons $1 $3)))
    (comp-exp ((math-exp EQ2 math-exp) (make-equal $1 $3))
              ((math-exp) $1))
    (math-exp ((math-exp + term) (make-sum $1 $3))
              ((math-exp - term) (make-diff $1 $3))
              ((term) $1))
    (term ((term * factor) (make-prod $1 $3))
          ((term / factor) (make-quo $1 $3))
          ((factor) $1))
    (factor ((ID) $1)
            ((NUM) $1)
            ((- factor) (make-neg $2)))
    (simple ((ID) $1)
            ((simple DECIMAL ID) (make-access $1 $3))
            ((simple OP actuals CP) (make-funcall $1 $3))
            ((OP exp CP) $2))
    (actuals (() null)
             ((non-emp-actuals) $1))
    (non-emp-actuals ((exp) (list $1))
                     ((exp COMMA non-emp-actuals) (cons $1 $3)))
    (formals (() null)
             ((non-emp-formals) $1))
    (non-emp-formals ((ID) (list $1))
                     ((ID COMMA non-emp-formals) (cons $1 $3))))))

(let* ((example "let x = 1+1 + 3 * 4, y = 0 in {y = 14; x == y}")
       (i (open-input-string example)))
    (equal? (parse-lang (lambda () (get-token i)))
          '(program
            ()
            (with-bindings
             ((x (sum (sum 1 1) (prod 3 4))) (y 0))
             (sequence (assign! y 14) (equality? x y))))))

;my sequence returns like this: (sequence ((assign! y 14) (equality? x y)))
  
;
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
;;                 (n)
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
 ; method move(dx, dy){
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

 
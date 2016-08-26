;;; Author: Arthur Nunes-Harwitt

(require (lib "yacc.ss" "parser-tools")
         (lib "lex.ss" "parser-tools")
         (prefix : (lib "lex-sre.ss" "parser-tools")))

(require (lib "pretty.ss"))

(define-tokens value-tokens (NUM ID))

(define-empty-tokens op-tokens
  (BEGIN
   END
   SEMI
   EQ1
   EQ2
   IF
   THEN
   ELSE
   OP
   CP
   COMMA
   LET
   IN
   BSLASH
   ARROW
   PROCS
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
 (ident (:+ letter))
 (number (:+ digit)))

;get-token: inputPort -> token
(define get-token
  (lexer
   ((eof) 'EOF)
   ("if" 'IF)
   ("then" 'THEN)
   ("else" 'ELSE)
   ("let" 'LET)
   ("in" 'IN)
   ("\\" 'BSLASH) ;just one, it looks like two because \ is an escape character
   ("->" 'ARROW)
   ("procedures" 'PROCS)
   ("{" 'BEGIN)
   ("}" 'END)
   ("(" 'OP)
   (")" 'CP)
   (";" 'SEMI)
   ("," 'COMMA)
   ("=" 'EQ1)
   ("==" 'EQ2)
   ("+" '+)
   ("-" '-)
   ("*" '*)
   ("/" '/)
   (number (token-NUM (string->number lexeme)))
   (ident (token-ID (string->symbol lexeme)))
   (whitespace (get-token input-port))))


;; An expression (Exp) is one of the following.

;; a Number n
;; an Identifier x

;; a sum with parts e1 and e2,

;;   where e1 and e2 are expressions
;; a let with parts x, e1, and e2,

;;   where x is an identifier
;;   and e1 and e2 are expressions
;; an if with parts e1, e2, and e3,
;;   where e1, e2, and e3 are expressions
;; a proc with parts x and e
;;   where x is an identifier and e is an expression
;; a funcall with parts e1 and e2
;;   where e1 and e2 are expressions
;; an assignment with parts x and e
;;   where x is an identifier and e is an expression
;; functions for associated with each part: predicate, constructor, selectors.
;; Number is a Scheme number
;; Identifier is a Scheme symbol


; Predicates

; ident?: Any -> Bool
(define ident? symbol?)

; sum?: Any -> Bool
(define (sum? a) (and (pair? a) (eq? (car a) '+)))

; diff?: Any -> Bool
(define (diff? a) (and (pair? a) (eq? (car a) '-)))

; prod?: Any -> Bool
(define (prod? a) (and (pair? a) (eq? (car a) '*)))

; quo?: Any -> Bool
(define (quo? a) (and (pair? a) (eq? (car a) '/)))

; neg?: Any -> Bool
(define (neg? a) (and (pair? a) (eq? (car a) 'neg)))

; equality?: Any -> Bool
(define (equality? a) (and (pair? a) (eq? (car a) '=)))

; let?: Any -> Bool
(define (let? a) (and (pair? a) (eq? (car a) 'let)))

; if?: Any -> Bool
(define (if? a) (and (pair? a) (eq? (car a) 'if)))

; proc?: Any -> Bool
(define (proc? a) (and (pair? a) (eq? (car a) 'proc)))

; assign?: Any -> Bool
;(define (assign? a) (and (pair? a) (eq? (car a) 'assign!)))

; funcall? Any -> Bool
(define (funcall? a) (and (pair? a) (eq? (car a) 'funcall)))

;constructors

; make-sum: Exp * Exp -> SumExp
(define (make-sum exp1 exp2)
  (list '+ exp1 exp2))

; make-diff: Exp * Exp -> DiffExp
(define (make-diff exp1 exp2)
  (list '- exp1 exp2))

; make-prod: Exp * Exp -> ProdExp
(define (make-prod exp1 exp2)
  (list '* exp1 exp2))

; make-quo: Exp * Exp -> QuoExp
(define (make-quo exp1 exp2)
  (list '/ exp1 exp2))

; make-neg: Exp -> NegExp
(define (make-neg exp1)
  (list 'neg exp1))

; make-equality: Exp * Exp -> EqualityExp
(define (make-equality exp1 exp2)
  (list '= exp1 exp2))

; make-let: Identifier*Exp * Exp -> LetExp
; Identifier*Exp is represented as a two element list
(define (make-let var exp1 exp2)
  (list 'let var exp1 exp2))

; make-if: Exp * Exp * Exp -> IfExp
(define (make-if exp1 exp2 exp3)
  (list 'if exp1 exp2 exp3))

; make-proc: Identifier * Exp -> ProcExp
(define (make-proc var exp)
  (list 'proc var exp))

; make-funcall: Exp * Exp -> FuncallExp
(define (make-funcall exp1 exp2)
  (list 'funcall exp1 exp2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; make-letstar: Identifier*Exp * Exp -> LetStarExp
(define (make-letstar exp1 var)
  (if (null? exp1) 
      var
      (make-let (car (car exp1)) (cadr (car exp1)) (make-letstar (cdr exp1) var))))

(define (make-seq exp1)
  (if (null? exp1)
      '()
      (if (null? (cdr exp1))
          (car exp1)
          (make-let '*temp* (car exp1) (make-seq (cdr exp1))))))

(define (make-curried-proc exp1 var)
  (if (null? exp1) 
      (make-proc '*temp* var)
      (if (null? (cdr exp1))
          (make-proc (car exp1) var)
          (make-proc (car exp1) (make-curried-proc (cdr exp1) var)))))

(define (make-curried-funcall func vars)
  (if (null? vars)
      (make-funcall func '0)
      (if (null? (cdr vars))
          (make-funcall func (car vars))
          (make-curried-funcall (make-funcall func (car vars)) (cdr vars)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selectors

; arg1: SumExp -> Exp
(define (arg1 e) (car (cdr e)))

; arg2: SumExp -> Exp
(define (arg2 e) (car (cdr (cdr e))))

; let-var: LetExp -> Identifier
(define let-var cadr)

; let-exp: LetExp -> Exp
(define let-exp caddr)

; let-body: LetExp -> Exp
(define let-body cadddr)

; if-exp1: IfExp -> Exp
(define if-exp1 cadr)

; if-exp2: IfExp -> Exp
(define if-exp2 caddr)

; if-exp3: IfExp -> Exp
(define if-exp3 cadddr)

; proc-var: ProcExp -> Identifier
(define proc-var cadr)

; proc-exp: ProcExp -> Exp
(define proc-exp caddr)

; funcall-rator: FuncallExp -> Exp
(define funcall-rator cadr)

; funcall-rand: FuncallExp -> Exp
(define funcall-rand caddr)

; neg-exp: NegExp -> Exp
(define neg-exp cadr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An environment (Env) is a Scheme function from Identifier to Loc.
;; where a Loc is a NatNum

(define empty-env
  (lambda (var) (error 'empty-env "variable undefined")))

; apply-env: Env * Identifier -> Loc
(define (apply-env env var) (env var))

; extend-env: Env * Identifier * Loc -> Env
(define (extend-env env var val)
  (lambda (var2)
    (if (eq? var var2)
        val
        (env var2))))

;; continuations
(define init-k (lambda (v) v))

(define (boolify exp) exp)

; meaning: Exp * Env * Val
(define (meaning exp env k)
  (cond ((number? exp) (k exp))
        ((ident? exp)((apply-env env exp) k))
        ((sum? exp)
         (meaning (arg1 exp)
                  env
                  (lambda (v1)
                    (meaning (arg2 exp)
                             env
                             (lambda (v2)
                               (k (+ v1 v2)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
        ((diff? exp)
         (meaning (arg1 exp)
                  env
                  (lambda (v1)
                    (meaning (arg2 exp)
                             env
                             (lambda (v2)
                               (k (- v1 v2)))))))
        ((prod? exp)
         (meaning (arg1 exp)
                  env
                  (lambda (v1)
                    (meaning (arg2 exp)
                             env
                             (lambda (v2)
                               (k (* v1 v2)))))))
        ((quo? exp)
         (meaning (arg1 exp)
                  env
                  (lambda (v1)
                    (meaning (arg2 exp)
                             env
                             (lambda (v2)
                               (k (/ v1 v2)))))))
        
        ((neg? exp)
         (meaning (arg1 exp)
                  env
                  (lambda (v1)
                    (k (* -1 v1)))))
        ((equality? exp)
         (meaning (arg1 exp)
                  env
                  (lambda (v1)
                    (meaning (arg2 exp)
                             env
                             (lambda (v2)
                               (k (eqv? v1 v2)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
        
        ((let? exp)
         (meaning (let-body exp)
                  (extend-env env (let-var exp) 
                              (lambda (k2)
                                (meaning (let-exp exp)
                                         env
                                         k2))) k))
                            
        ((if? exp)
         (meaning (if-exp1 exp)
                  env
                  (lambda (v)
                    (if (boolify v)
                        (meaning (if-exp2 exp) env k)
                        (meaning (if-exp3 exp) env k)))))        
        
        ((proc? exp)
         (k (lambda (v k2)
                (meaning (proc-exp exp)
                         (extend-env env (proc-var exp) v) k2))))
        
        ((funcall? exp)
         (meaning (funcall-rator exp)
                  env
                  (lambda (f)
                    (f (lambda (k2)
                         (meaning (funcall-rand exp) env k2)) k))))
      (else (error 'meaning "Unknown expression"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; parse-lang: (() -> token) -> SmallLangExp
(define parse-lang
  (parser
   (start exp)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (error 'parse-lang "error occurred, ~v ~v ~v" a b c)))
   (grammar
    (exp ((BEGIN exps END) (make-seq $2))
         ((LET let-defs IN exp) (make-letstar $2 $4))
         ((PROCS rec-defs IN exp) (make-procs $2 $4))
         ((IF exp THEN exp ELSE exp) (make-if $2 $4 $6))
         ((BSLASH formals BSLASH ARROW  exp ) (make-curried-proc $2 $5))
         ((ID EQ1 exp) (make-assign $1 $3))
         ((comp-exp) $1))
    (exps ((exp) (list $1))
          ((exp SEMI exps) (cons $1 $3)))
    (let-def ((ID EQ1 exp) (list $1 $3)))
    (let-defs ((let-def) (list $1))
              ((let-def COMMA let-defs) (cons $1 $3)))
    (rec-def ((ID OP formals CP EQ1 exp) (list $1 (make-curried-proc $3 $6))))
    (rec-defs ((rec-def) (list $1))
              ((rec-def COMMA rec-defs) (cons $1 $3)))
    (comp-exp ((math-exp EQ2 math-exp) (make-equality $1 $3))
              ((math-exp) $1))
    (math-exp ((math-exp + term) (make-sum $1 $3))
              ((math-exp - term) (make-diff $1 $3))
              ((term) $1))
    (term ((term * factor) (make-prod $1 $3))
          ((term / factor) (make-quo $1 $3))
          ((factor) $1))
    (factor ((simple) $1)
            ((NUM) $1)
            ((- factor) (make-neg $2))
            ((simple OP actuals CP) (make-curried-funcall $1 $3)))
    (simple ((ID) $1)
            ((OP exp CP) $2))
    (actuals (() null)
             ((actualsNE) (reverse $1)))
    (actualsNE ((exp) (list $1))
               ((actualsNE COMMA exp) (cons $3 $1)))
    (formals (() null)
             ((formalsNE) (reverse $1)))
    (formalsNE ((ID) (list $1))
               ((formalsNE COMMA ID) (cons $3 $1))))))




;Part B Tests
(let* ((example "let x = 1/0 in 3")
       (i (open-input-string example)))
    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k) 3))



(let* ((example "let x = z in 3")
       (i (open-input-string example)))
    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k) 3))



(let* ((example "(\\x,y\\->x)(1,1/0)")
       (i (open-input-string example)))
    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k) 1))


(let* ((example "(\\x,y\\->x)(1,z)")
      (i (open-input-string example)))
    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k) 1))
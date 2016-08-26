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
   EOF))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ "A" "Z"))
  (letter (:or lower-letter upper-letter))
  (digit (:/ "0" "9"))
  
  (digits (:+ digit)))

  (ident (:+ letter))

;get-token: inputPort -> token
(define get-token
  (lexer
   ((eof) ’EOF)
   ("let" ’LET)
   ("in" ’IN)
   ("(" ’OP)
   (")" ’CP)
   ("," ’COMMA)
   ("=" ’EQ1)
   ("+" ’+)
   ("-" ’-)
   ("*" ’*)
   ("/" ’/)
   (number (token-NUM (string->number lexeme)))
   (ident (token-ID (string->symbol lexeme)))
   (whitespace (get-token input-port))))

;;; data definitions


;; A large language expression (LargeLangExp) is one of the following.
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


; make-sum: SmallLangExp * SmallLangExp -> SumExp
(define (make-sum exp1 exp2)
  (list ’sum exp1 exp2))

; (equal? (make-sum 2 3) ’(sum 2 3))

; make-prod: SmallLangExp * SmallLangExp -> ProdExp
(define (make-prod exp1 exp2)
  (list ’prod exp1 exp2))
; (equal? (make-prod 2 3) ’(prod 2 3))

; make-quo: SmallLangExp * SmallLangExp -> QuoExp
(define (make-quo exp1 exp2)
  (list ’quo exp1 exp2))

; (equal? (make-quo 2 3) ’(quo 2 3))

; make-neg: SmallLangExp -> NegExp
(define (make-neg exp)
  (list ’neg exp))

; (equal? (make-neg 2) ’(neg 2))

; make-let: Listof(Identifier*SmallLangExp) * SmallLangExp -> BindingExp
; Identifier*SmallLangExp is represented as a two element list
(define (make-let defs exp)
  (list ’with-bindings defs exp))

; (equal? (make-let (list (list ’x 1) (list ’y 2)) 3) ’(with-bindings ((x 1) (y 2)) 3))


; make-neg: SmallLangExp -> NegExp
(define (make-program exp1 exp2)
  (list ’program exp1 exp2))

;(define (makeclass exp1 exp2 exp3 exp4)
;  (list ’class exp1 exp2 exp3 exp4))

;(define (make-method exp1 exp2)
;  (list ’method exp1 exp2))

(define (make-new exp1 exp2)
  (list ’new exp1 exp2))

(define (make-supercall exp1 exp2)
  (list ’super exp1 exp2))

(define (make-seq exp1)
  (list ’sequence exp1))

(define (make-procs exp1 exp2)
  (list ’procedures exp1 exp2))

(define (make-if exp1 exp2 exp3)
  (list ’if exp1 exp2 exp3))

(define (make-assign exp1 exp2)
  (list ’assign! exp1 exp2))

(define (make-equal exp1 exp2)
  (list ’equality? exp1 exp2))

(define (make-proc exp1 exp2)
  (list ’proc exp1 exp2))

(define (make-access exp1 exp2)
  (list ’send exp1 exp2))

(define (make-funcall exp1 exp2)
  (list ’funcall exp1 exp2))
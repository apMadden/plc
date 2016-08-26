



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; lexer/parser/meaning test


(let* ((example "2+3+4")
              (i (open-input-string example)))
   (parse-lang (lambda () (get-token i))) )
;(let* ((example "3+4")
;       (i (open-input-string example)))
;    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k empty-store) 9))

;(let* ((example "12/2")
;       (i (open-input-string example)))
;    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k empty-store) 4))
;(let* ((example "8/2")
;       (i (open-input-string example)))
;    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k empty-store) 4));

;(let* ((example "8-2-1")
;       (i (open-input-string example)))
;    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k empty-store) 6))
;(let* ((example "8-2")
;       (i (open-input-string example)))
;    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k empty-store) 6))

;(let* ((example "8*3*2")
;       (i (open-input-string example)))
;    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k empty-store) 48))
;(let* ((example "8*2")
 ;      (i (open-input-string example)))
;    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k empty-store) 15))


;(let* ((example "-5")
;       (i (open-input-string example)))
;    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k empty-store) -5))
;(let* ((example "-5")
;       (i (open-input-string example)))
;    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k empty-store) -8))

;(let* ((example "8==8")
;       (i (open-input-string example)))
;    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k empty-store) #t))
;Part B Tests
;(let* ((example "let x = 1/0 in 3")
;       (i (open-input-string example)))
;    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k) 3))



;(let* ((example "let x = z in 3")
;       (i (open-input-string example)))
;    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k) 3))



;(let* ((example "(\\x,y\\->x)(1,1/0)")
;       (i (open-input-string example)))
;    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k) 1))



;(let* ((example "(\\x,y\\->x)(1,z)")
;       (i (open-input-string example)))
;    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k) 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (ident? 'x)
; (eq? (sum? '(+ 2 3)) #t)
; (eq? (sum? '3) #f)
; (equal? (make-sum 2 3) '(+ 2 3))
; (eq? (let? '(let x 1 3)) #t)
; (eq? (let? '3) #f)
; (equal? (make-let 'x 1 3) '(let x 1 3))
; (if? (make-if 1 2 3))
; (equal? (make-if 1 2 3) '(if 1 2 3))
; (not (if? 5))
; (proc? '(proc x 1))
; (equal? (make-proc 'x 1) '(proc x 1))
; (not (proc? 3))
; (eq? (assign? '(assign! x 3)) #t)
; (eq? (assign? '3) #f)
; (funcall? '(funcall f 1))
; (not (funcall? 3))
; (equal? (make-funcall 'f 1) '(funcall f 1))
; (equal? (make-assign 'x 3) (assign! x 3))
; (equal? (make-diff 2 3) '(- 2 3))
; (equal? (make-prod 2 3) '(* 2 3))
; (equal? (make-quo 2 3) '(/ 2 3))
; (equal? (make-neg 3) '(neg 3))
; (equal? (make-equality 2 3) '(= 2 3))
; (= (arg1 (make-sum 2 3)) 2)
; (= (arg2 (make-sum 2 3)) 3)
; (eq? (let-var (make-let 'x 1 3)) 'x)
; (= (let-exp (make-let 'x 1 3)) 1)
; (= (let-body (make-let 'x 1 3)) 3)
; (= (if-exp1 (make-if 1 2 3)) 1)
; (= (if-exp2 (make-if 1 2 3)) 2)
; (= (if-exp3 (make-if 1 2 3)) 3)
; (eq? (proc-var (make-proc 'x 1)) 'x)
; (= (proc-exp (make-proc 'x 1)) 1)
; (eq? (funcall-rator (make-funcall 'f 1)) 'f)
; (= (funcall-rand (make-funcall 'f 1)) 1)
; (eq? (assign-var (make-assign 'x 3)) 'x)
; (= (assign-exp (make-assign 'x 3)) 3)


;(let *temp* a (let *temp* b (let *temp* c d)))


;(make-curried-proc '() '(* x 6))
;(make-curried-proc '(a) '(* x 6))
;(make-curried-proc '(a b c d) '(* x 6))

;(make-curried-funcall 'func '())
;(make-curried-funcall 'func '(a))
;(make-curried-funcall 'func '(a b c d))

;(equal? (make-letstar '() '(+ x 1)) '(+ x 1))
;(make-letstar '() '(* x 6))
;(make-letstar '((x 2)) '(* x x))
;(make-letstar '((x 5) (y 1) (z 2)) '(* z (+ x y)))
;(equal? (make-let 'x 1 3) '(let x 1 3))
      
;(make-seq '())
;(make-seq '(a))
;(make-seq '(a b c d))
;(let *temp* a (let *temp* b (let *temp* c d)))

;(make-curried-proc '() '(* x 6))
;(make-curried-proc '(a) '(* x 6))
;(make-curried-proc '(a b c d) '(* x 6))
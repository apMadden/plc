(define (union a b)
  (cond ((null? b) a)
        ((member (car b) a)
         (union a (cdr b)))
        (#t (union (cons (car b) a) (cdr b)))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((member (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(union '(1 2 3 4) '(5 2 6 3) )
(union-set '(1 2 3 4) '(5 2 6 3) )
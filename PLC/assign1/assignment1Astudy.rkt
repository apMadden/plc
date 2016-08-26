(define (2nd L)
  (car (cdr L)))

(define (one? L)
  (if (pair? L) (if (null? (cdr L)) #t #f) #f))
  

;(define (insertion-sort nums)


(cons '(a b) '(d e))

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

; 1) Function named 2nd which returns the second element of a given list.
(define (2nd L)
  (car (cdr L)))

; 2) Predicate named one? which returns true only if the given list contains a single element.
(define (one? L)
  (if (pair? L) (if (null? (cdr L)) #t #f) #f))

; 3) Function named insertion-sort which sorts a given list in ascending order.
;    This question requires the creation of an additional function called 'insert'
(define (insertion-sort nums)
  (if (pair? nums) (insert (car nums) (insertion-sort (cdr nums))) nums))

(define (insert element nums)
  (cond ((pair? nums) (cond ((> element (car nums))(cons (car nums)(insert element (cdr nums))))
                    (else (cons element nums))))
        (else (cons element '()))))

; 4) Function named index which returns the index of a given symbol in a given list, or #f if the
;    element doesn't exist in the given list.
; Existing Problems: doesn't handle empty lists or null symbols
(define (index sym syms)
  (if (null? sym)
      #f
      (if (eq? sym (car syms))
          0
          (if (pair? (cdr syms)) (+ 1 (index sym (cdr syms))) #f))))

; 5) A function named filter-by that returns a list containing the given list elements that satisfy
;    the given predicate.
(define (filter-by p L)
  (cond ((null? L) '())
      ((p (car L))
       (cons (car L) (filter-by p (cdr L))))
      (else (filter-by p (cdr L)))))
; I'm having trouble understanding what you want us to do to use filter-by in the quicksort example.
;
; I am unsure if we supposed to edit the actual qsort function, or only the find-less/more/same functions,
; or use the 'filter-by' function completely in place of the less/more/same functions.
;
; Below I attempt to use lambda expressions to solve the problem, but I have been unsuccesful so far and have run out of time.

(define (qsort L)
  (if (null? L) '()
      (let ((pivot (car L)))
        (let ((less (filter-by ((lambda (x y) (< x y) pivot) L)))
              (same (filter-by ((lambda (x y) (= x y) pivot) L)))
              (more (filter-by ((lambda (x y) (> x y) pivot) L))))
          (append (qsort less) same (qsort more))))))




;"2nd tests:"
;(2nd '(1 2 3))
;(2nd '(2 3 4))
;(2nd '(4 6))
;(2nd '(5 8 2 3 4 5))
;(2nd '(a b c d r))
;"one? tests:"
;(one? '())
;(one? '(0))
;(one? '(0 2 4 4))
;(one? '(0 2 3))
;(one? '(0 1))
;"insertion-sort tests:"
;(insertion-sort '(5 1 4 3 2 6 5))
;(insertion-sort '(1 2 3 4 5))
;(insertion-sort '(1 2))
;(insertion-sort '(2 1))
;(insertion-sort '(3 1 2))
;(insertion-sort '(2 1 3))
;(insertion-sort '(1 2 3))
;(insertion-sort '(1 3 2))
;"index tests:"
;(index 'x '(q r s x y z ))
;(index 'x '(x y z ))
;(index 'x '(q r x y z ))
;(index 'x '(q x y z ))
;(index 'x '(q))
;(index 'x '())
;"filter-by tests:
;(filter-by number? '(() a 5 (1 2 3) (x y z) b 10))
;(filter-by symbol? '(() a 5 (1 2 3) (x y z) b 10))
;(filter-by pair? '(() a 5 (1 2 3) (x y z) b 10))
;(filter-by list? '(() a 5 (1 2 3) (x y z) b 10))
;(filter-by empty? '(() a 5 (1 2 3) (x y z) b 10))
;(define (merge ordered1 ordered2)
;      (if (null? ordered1) ordered2
;          (if (null? ordered2) ordered1
;              (cons (car ordered1) (cons (car ordered2) (merge (cdr ordered1) (cdr ordered2)))))))
  (define (merge left right)
    (cond ((null? left) right)
     ((null? right) left)
     ((> (car left) (car right))
      (cons (car right)
            (merge left (cdr right))))
     (else
      (cons (car left)
            (merge (cdr left) right)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge-sort nums)
  (define (merge left right)
    (cond
     ((null? left)
      right)
     ((null? right)
      left)
     ((> (car left) (car right))
      (cons (car right)
            (merge left (cdr right))))
     (else
      (cons (car left)
            (merge (cdr left) right)))))
  (define (take nums n)
    (if (zero? n)
      (list)
      (cons (car nums)
            (take (cdr nums) (- n 1)))))
  (let ((half (quotient (length nums) 2)))
    (if (zero? half)
      nums
      (merge (merge-sort (take      nums half))
             (merge-sort (list-tail nums half))))))




(merge '(1 4 6) '(2 5 7))
(merge '(4 8 12) '(1 6 9 30))
                                                       
(merge-sort '(4 5 6 2 3 4 1))
(merge-sort '(2 3 4 1))
(merge-sort '(4 5 6 2 3 4 1 3))

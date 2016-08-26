;;; Author: Arthur Nunes-Harwitt

(let* ((example "let x = -(1+1) + 3 * 4, y = 0 in {y = 14; x == y}")
       (i (open-input-string example)))
  (equal? (parse-lang (lambda () (get-token i)))
          '(program
            ()
            (with-bindings
             ((x (sum (neg (sum 1 1)) (prod 3 4))) (y 0))
             (sequence (assign! y 14) (equality? x y))))))

(let* ((example 
"let pred = \\k\\->k-1 
  in procedures f(n) = if n == 0
                       then 1 
                       else n * f(pred(n)) 
      in f(4+1)
")
       (i (open-input-string example)))
  (equal? (parse-lang (lambda () (get-token i)))
          '(program
            ()
            (with-bindings
             ((pred (proc (k) (diff k 1))))
             (procedures
              ((f
                (proc
                 (n)
                 (if (equality? n 0) 1 (prod n (funcall f (funcall pred n)))))))
              (funcall f (sum 4 1)))))))


(let* ((example 
"class point extends object{
  field x
  field y
  method init(initx, inity){
   x = initx;
   y = inity
  }
  method move(dx, dy){
   x = x + dx;
   y = y + dy
  }
}
let ob = new point(2+3, 1+4*7) in
  ob.move(0.1,3)
")
       (i (open-input-string example)))
  (equal? (parse-lang (lambda () (get-token i)))
          '(program
            ((class point object
                    (x y)
                    ((method
                      init
                      (initx inity)
                      (sequence (assign! x initx) (assign! y inity)))
                     (method
                      move
                      (dx dy)
                      (sequence (assign! x (sum x dx)) (assign! y (sum y dy)))))))
            (with-bindings
             ((ob (new point (sum 2 3) (sum 1 (prod 4 7)))))
             (funcall (send ob move) 0.1 3)))))
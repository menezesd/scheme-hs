;; Test file for the Scheme interpreter

;; Basic arithmetic
(display "Testing arithmetic: ")
(display (+ 1 2 3 4 5))
(newline)

;; Factorial
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(display "factorial 10: ")
(display (factorial 10))
(newline)

;; Fibonacci
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(display "fib 10: ")
(display (fib 10))
(newline)

;; Map
(display "map square '(1 2 3 4 5): ")
(display (map (lambda (x) (* x x)) '(1 2 3 4 5)))
(newline)

;; Let bindings
(display "let: ")
(display (let ((x 10) (y 20)) (+ x y)))
(newline)

;; Let*
(display "let*: ")
(display (let* ((x 10) (y (+ x 5))) (* x y)))
(newline)

;; Letrec (for mutual recursion)
(display "letrec (even/odd): ")
(display (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                  (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
           (even? 10)))
(newline)

;; Quasiquote
(define x 42)
(display "quasiquote: ")
(display `(a b ,x d))
(newline)

;; Vectors
(display "vector: ")
(display (vector 1 2 3 4 5))
(newline)

(display "vector-ref: ")
(display (vector-ref (vector 10 20 30) 1))
(newline)

;; Cond with =>
(display "cond =>: ")
(display (cond ((assoc 'b '((a 1) (b 2) (c 3))) => cadr)
               (else 'not-found)))
(newline)

;; Do loop
(display "do loop: ")
(display (do ((i 0 (+ i 1))
              (sum 0 (+ sum i)))
             ((= i 10) sum)))
(newline)

;; Apply
(display "apply: ")
(display (apply + '(1 2 3 4 5)))
(newline)

;; Higher order functions from stdlib
(display "filter (odd? 1-10): ")
(display (filter odd? (iota 10)))
(newline)

(display "fold-left: ")
(display (fold-left + 0 '(1 2 3 4 5)))
(newline)

;; Closures
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define counter (make-counter))
(display "counter: ")
(display (counter))
(display " ")
(display (counter))
(display " ")
(display (counter))
(newline)

(display "All tests completed!")
(newline)

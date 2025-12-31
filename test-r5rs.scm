;; R5RS Comprehensive Test Suite
;; Tests all newly implemented procedures

(display "=== R5RS Comprehensive Tests ===\n\n")

;; ============================================================================
;; Deep car/cdr accessors
;; ============================================================================
(display "--- Deep car/cdr Accessors ---\n")
(define nested '((1 2) (3 4) (5 6)))
(display "(caar nested) = ") (display (caar nested)) (newline)
(display "(cadr nested) = ") (display (cadr nested)) (newline)
(display "(caddr nested) = ") (display (caddr nested)) (newline)

(define deep '((((1 2) 3) 4) 5))
(display "(caaaar deep) = ") (display (caaaar deep)) (newline)  ; should be 1

;; ============================================================================
;; String operations
;; ============================================================================
(display "\n--- String Operations ---\n")
(display "(string-set! \"hello\" 0 #\\H) = ")
(display (string-set! "hello" 0 #\H)) (newline)
(display "(string-fill! \"test\" #\\x) = ")
(display (string-fill! "test" #\x)) (newline)

;; ============================================================================
;; Vector operations
;; ============================================================================
(display "\n--- Vector Operations ---\n")
(define v (vector 1 2 3 4 5))
(display "(vector 1 2 3 4 5) = ") (display v) (newline)
(display "(vector-fill! v 0) = ")
(display (vector-fill! v 0)) (newline)

;; ============================================================================
;; Port operations
;; ============================================================================
(display "\n--- Port Operations ---\n")
(display "(port? (current-input-port)) = ")
(display (port? (current-input-port))) (newline)
(display "(input-port? (current-input-port)) = ")
(display (input-port? (current-input-port))) (newline)
(display "(output-port? (current-output-port)) = ")
(display (output-port? (current-output-port))) (newline)
(display "(eof-object? #f) = ")
(display (eof-object? #f)) (newline)

;; ============================================================================
;; eval
;; ============================================================================
(display "\n--- Eval ---\n")
(display "(eval '(+ 1 2 3)) = ")
(display (eval '(+ 1 2 3))) (newline)
(display "(eval '(* 2 (+ 3 4))) = ")
(display (eval '(* 2 (+ 3 4)))) (newline)

;; ============================================================================
;; call/cc
;; ============================================================================
(display "\n--- call/cc ---\n")

;; Simple escape continuation
(define result1
  (+ 1 (call/cc (lambda (k)
                  (+ 2 (k 3))))))
(display "(+ 1 (call/cc (lambda (k) (+ 2 (k 3))))) = ")
(display result1) (newline)

;; Early return from loop
(define (find-first pred lst)
  (call/cc
   (lambda (return)
     (for-each (lambda (x)
                 (if (pred x) (return x)))
               lst)
     #f)))

(display "(find-first even? '(1 3 5 6 7)) = ")
(display (find-first even? '(1 3 5 6 7))) (newline)

;; ============================================================================
;; values and call-with-values
;; ============================================================================
(display "\n--- Multiple Values ---\n")
(display "(values 1 2 3) = ")
(display (values 1 2 3)) (newline)

;; ============================================================================
;; Environment procedures
;; ============================================================================
(display "\n--- Environment Procedures ---\n")
(display "(scheme-report-environment 5) = ")
(display (scheme-report-environment 5)) (newline)
(display "(null-environment 5) = ")
(display (null-environment 5)) (newline)
(display "(interaction-environment) = ")
(display (interaction-environment)) (newline)

;; ============================================================================
;; set-car! and set-cdr!
;; ============================================================================
(display "\n--- Pair Mutation ---\n")
(define pair1 (cons 1 2))
(display "original pair: ") (display pair1) (newline)
(display "(set-car! pair1 'a) = ")
(display (set-car! pair1 'a)) (newline)
(define pair2 (list 1 2 3))
(display "original list: ") (display pair2) (newline)
(display "(set-cdr! pair2 '(x y)) = ")
(display (set-cdr! pair2 '(x y))) (newline)

;; ============================================================================
;; delay/force
;; ============================================================================
(display "\n--- Delay/Force ---\n")
(define promise1 (delay (begin (display "evaluated!\n") 42)))
(display "Promise created.\n")
(display "(force promise1) = ") (display (force promise1)) (newline)

;; ============================================================================
;; dynamic-wind
;; ============================================================================
(display "\n--- Dynamic-wind ---\n")
(define counter 0)
(dynamic-wind
  (lambda () (set! counter (+ counter 1)) (display "before\n"))
  (lambda () (display "body\n") 'result)
  (lambda () (set! counter (+ counter 1)) (display "after\n")))
(display "counter = ") (display counter) (newline)

;; ============================================================================
;; Numeric tower (from previous tests)
;; ============================================================================
(display "\n--- Numeric Tower ---\n")
(display "(+ 1/2 1/3) = ") (display (+ 1/2 1/3)) (newline)
(display "(sqrt -1) = ") (display (sqrt -1)) (newline)
(display "(exact? 42) = ") (display (exact? 42)) (newline)
(display "(inexact? 3.14) = ") (display (inexact? 3.14)) (newline)

;; ============================================================================
;; Utility functions from stdlib
;; ============================================================================
(display "\n--- Utility Functions ---\n")
(display "(filter even? '(1 2 3 4 5)) = ")
(display (filter even? '(1 2 3 4 5))) (newline)
(display "(fold-left + 0 '(1 2 3 4 5)) = ")
(display (fold-left + 0 '(1 2 3 4 5))) (newline)
(display "(any even? '(1 3 5)) = ")
(display (any even? '(1 3 5))) (newline)
(display "(any even? '(1 2 3)) = ")
(display (any even? '(1 2 3))) (newline)
(display "(all even? '(2 4 6)) = ")
(display (all even? '(2 4 6))) (newline)
(display "(range 1 5) = ")
(display (range 1 5)) (newline)
(display "(take 3 '(1 2 3 4 5)) = ")
(display (take 3 '(1 2 3 4 5))) (newline)
(display "(drop 3 '(1 2 3 4 5)) = ")
(display (drop 3 '(1 2 3 4 5))) (newline)
(display "(zip '(1 2 3) '(a b c)) = ")
(display (zip '(1 2 3) '(a b c))) (newline)
(display "(sum '(1 2 3 4 5)) = ")
(display (sum '(1 2 3 4 5))) (newline)
(display "(product '(1 2 3 4 5)) = ")
(display (product '(1 2 3 4 5))) (newline)

(display "\n=== All R5RS tests completed! ===\n")

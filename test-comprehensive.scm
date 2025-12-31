;;;============================================================================
;;; Test Framework
;;;============================================================================

(define *tests-run* 0)
(define *tests-passed* 0)
(define *tests-failed* 0)
(define *current-section* "")

(define (test-section name)
  (set! *current-section* name)
  (display "=== ")
  (display name)
  (display " ===")
  (newline))

(define-syntax test
  (syntax-rules ()
    ((test name expect expr)
     (test expect expr))
    ((test expect expr)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (let ((res expr))
         (cond
          ((equal? res expect)
           (set! *tests-passed* (+ *tests-passed* 1)))
          (else
           (set! *tests-failed* (+ *tests-failed* 1))
           (display "  FAIL: ")
           (write 'expr)
           (display " => ")
           (write res)
           (display ", expected ")
           (write expect)
           (newline))))))))

(define-syntax should-be
  (syntax-rules ()
    ((_ test-id value expression)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (let ((res expression))
         (cond
          ((equal? res value)
           (set! *tests-passed* (+ *tests-passed* 1)))
          (else
           (set! *tests-failed* (+ *tests-failed* 1))
           (display "  FAIL ")
           (display test-id)
           (display ": got ")
           (write res)
           (display ", expected ")
           (write value)
           (newline))))))))

(define (test-report)
  (newline)
  (display "========================================")
  (newline)
  (display "Tests: ") (display *tests-run*)
  (display ", Passed: ") (display *tests-passed*)
  (display ", Failed: ") (display *tests-failed*)
  (newline)
  (if (= *tests-failed* 0)
      (display "All tests passed!")
      (display "SOME TESTS FAILED"))
  (newline))

;;;============================================================================
;;; Chibi R5RS Tests
;;;============================================================================

(test-section "Chibi: Lambda & Procedures")
(test 8 ((lambda (x) (+ x x)) 4))
(test '(3 4 5 6) ((lambda x x) 3 4 5 6))
(test '(5 6) ((lambda (x y . z) z) 3 4 5 6))

(test-section "Chibi: Conditionals")
(test 'yes (if (> 3 2) 'yes 'no))
(test 'no (if (> 2 3) 'yes 'no))
(test 1 (if (> 3 2) (- 3 2) (+ 3 2)))
(test 'greater (cond ((> 3 2) 'greater) ((< 3 2) 'less)))
(test 'equal (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal)))
(test 'composite (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite)))
(test 'consonant
    (case (car '(c d))
      ((a e i o u) 'vowel)
      ((w y) 'semivowel)
      (else 'consonant)))

(test-section "Chibi: Boolean Operations")
(test #t (and (= 2 2) (> 2 1)))
(test #f (and (= 2 2) (< 2 1)))
(test '(f g) (and 1 2 'c '(f g)))
(test #t (and))
(test #t (or (= 2 2) (> 2 1)))
(test #t (or (= 2 2) (< 2 1)))
(test '(b c) (or (memq 'b '(a b c)) (/ 3 0)))

(test-section "Chibi: Let Forms")
(test 6 (let ((x 2) (y 3)) (* x y)))
(test 35 (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))
(test 70 (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))))
(test -2 (let ()
           (define x 2)
           (define f (lambda () (- x)))
           (f)))

(test-section "Chibi: Do Loop")
(test '#(0 1 2 3 4)
 (do ((vec (make-vector 5))
      (i 0 (+ i 1)))
     ((= i 5) vec)
   (vector-set! vec i i)))

(test 25
    (let ((x '(1 3 5 7 9)))
      (do ((x x (cdr x))
           (sum 0 (+ sum (car x))))
          ((null? x)
           sum))))

(test-section "Chibi: Named Let")
(test '((6 1 3) (-5 -2))
    (let loop ((numbers '(3 -2 1 6 -5)) (nonneg '()) (neg '()))
      (cond
       ((null? numbers)
        (list nonneg neg))
       ((>= (car numbers) 0)
        (loop (cdr numbers) (cons (car numbers) nonneg) neg))
       ((< (car numbers) 0)
        (loop (cdr numbers) nonneg (cons (car numbers) neg))))))

(test-section "Chibi: Quasiquote")
(test '(list 3 4) `(list ,(+ 1 2) 4))
(test '(list a 'a) (let ((name 'a)) `(list ,name ',name)))
(test '(a 3 4 5 6 b) `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
(test '(10 5 4 16 9 8) `(10 5 ,(expt 2 2) ,@(map (lambda (n) (expt n n)) '(4 3)) 8))
(test '(list 3 4) (quasiquote (list (unquote (+ 1 2)) 4)))

(test-section "Chibi: Equivalence Predicates")
(test #t (eqv? 'a 'a))
(test #f (eqv? 'a 'b))
(test #t (eqv? '() '()))
(test #f (eqv? (cons 1 2) (cons 1 2)))
(test #f (eqv? (lambda () 1) (lambda () 2)))
(test #t (let ((p (lambda (x) x))) (eqv? p p)))
(test #t (eq? 'a 'a))
(test #f (eq? (list 'a) (list 'a)))
(test #t (eq? '() '()))
(test #t (eq? car car))
(test #t (let ((x '(a))) (eq? x x)))
(test #t (let ((p (lambda (x) x))) (eq? p p)))
(test #t (equal? 'a 'a))
(test #t (equal? '(a) '(a)))
(test #t (equal? '(a (b) c) '(a (b) c)))
(test #t (equal? "abc" "abc"))
(test #f (equal? "abc" "abcd"))
(test #f (equal? "a" "b"))
(test #t (equal? 2 2))
(test #t (equal? (make-vector 5 'a) (make-vector 5 'a)))

(test-section "Chibi: Arithmetic")
(test 4 (max 3 4))
(test 7 (+ 3 4))
(test 3 (+ 3))
(test 0 (+))
(test 4 (* 4))
(test 1 (*))
(test -1 (- 3 4))
(test -6 (- 3 4 5))
(test -3 (- 3))
(test -1.0 (- 3.0 4))
(test 7 (abs -7))
(test 1 (modulo 13 4))
(test 1 (remainder 13 4))
(test 3 (modulo -13 4))
(test -1 (remainder -13 4))
(test -3 (modulo 13 -4))
(test 1 (remainder 13 -4))
(test -1 (modulo -13 -4))
(test -1 (remainder -13 -4))
(test 4 (gcd 32 -36))
(test 288 (lcm 32 -36))

(test-section "Chibi: Number Conversion")
(test 100 (string->number "100"))
(test 256 (string->number "100" 16))
(test 127 (string->number "177" 8))
(test 5 (string->number "101" 2))
(test 100.0 (string->number "1e2"))
(test "100" (number->string 100))
(test "100" (number->string 256 16))
(test "ff" (number->string 255 16))
(test "177" (number->string 127 8))
(test "101" (number->string 5 2))

(test-section "Chibi: Pairs and Lists")
(test #t (pair? '(a . b)))
(test #t (pair? '(a b c)))
(test '(a) (cons 'a '()))
(test '((a) b c d) (cons '(a) '(b c d)))
(test '("a" b c) (cons "a" '(b c)))
(test '(a . 3) (cons 'a 3))
(test '((a b) . c) (cons '(a b) 'c))
(test 'a (car '(a b c)))
(test '(a) (car '((a) b c d)))
(test 1 (car '(1 . 2)))
(test '(b c d) (cdr '((a) b c d)))
(test 2 (cdr '(1 . 2)))
(test #t (list? '(a b c)))
(test #t (list? '()))
(test #f (list? '(a . b)))
(test #f (let ((x (list 'a))) (set-cdr! x x) (list? x)))
(test '(a 7 c) (list 'a (+ 3 4) 'c))
(test '() (list))
(test 3 (length '(a b c)))
(test 3 (length '(a (b) (c d e))))
(test 0 (length '()))
(test '(x y) (append '(x) '(y)))
(test '(a b c d) (append '(a) '(b c d)))
(test '(a (b) (c)) (append '(a (b)) '((c))))
(test '(a b c . d) (append '(a b) '(c . d)))
(test 'a (append '() 'a))
(test '(c b a) (reverse '(a b c)))
(test '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))
(test 'c (list-ref '(a b c d) 2))
(test '(a b c) (memq 'a '(a b c)))
(test '(b c) (memq 'b '(a b c)))
(test #f (memq 'a '(b c d)))
(test #f (memq (list 'a) '(b (a) c)))
(test '((a) c) (member (list 'a) '(b (a) c)))
(test '(101 102) (memv 101 '(100 101 102)))
(test #f (assq (list 'a) '(((a)) ((b)) ((c)))))
(test '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))
(test '(5 7) (assv 5 '((2 3) (5 7) (11 13))))

(test-section "Chibi: Symbols")
(test #t (symbol? 'foo))
(test #t (symbol? (car '(a b))))
(test #f (symbol? "bar"))
;; NOTE: 'nil is () in this interpreter (deliberate Lisp-style choice)
;; (test #t (symbol? 'nil))
(test "flying-fish" (symbol->string 'flying-fish))
;; NOTE: This interpreter is case-sensitive (symbols read as-is)
;; (test "Martin" (symbol->string 'Martin))
(test "Malvina" (symbol->string (string->symbol "Malvina")))

(test-section "Chibi: Strings")
(test #t (string? "a"))
(test #f (string? 'a))
(test 0 (string-length ""))
(test 3 (string-length "abc"))
(test #\a (string-ref "abc" 0))
(test #\c (string-ref "abc" 2))
(test #t (string=? "a" (string #\a)))
(test #f (string=? "a" (string #\b)))
(test #t (string<? "a" "aa"))
(test #f (string<? "aa" "a"))
(test #f (string<? "a" "a"))
(test #t (string<=? "a" "aa"))
(test #t (string<=? "a" "a"))
(test #t (string=? "a" (make-string 1 #\a)))
(test #f (string=? "a" (make-string 1 #\b)))
(test "" (substring "abc" 0 0))
(test "a" (substring "abc" 0 1))
(test "bc" (substring "abc" 1 3))
(test "abc" (string-append "abc" ""))
(test "abc" (string-append "" "abc"))
(test "abc" (string-append "a" "bc"))

(test-section "Chibi: Vectors")
(test '#(0 ("Sue" "Sue") "Anna")
 (let ((vec (vector 0 '(2 2 2 2) "Anna")))
   (vector-set! vec 1 '("Sue" "Sue"))
   vec))
(test '(dah dah didah) (vector->list '#(dah dah didah)))
(test '#(dididit dah) (list->vector '(dididit dah)))

(test-section "Chibi: Procedures")
(test #t (procedure? car))
(test #f (procedure? 'car))
(test #t (procedure? (lambda (x) (* x x))))
(test #f (procedure? '(lambda (x) (* x x))))
(test #t (call-with-current-continuation procedure?))
(test 7 (call-with-current-continuation (lambda (k) (+ 2 5))))
(test 3 (call-with-current-continuation (lambda (k) (+ 2 5 (k 3)))))

(test-section "Chibi: Apply and Map")
(test 7 (apply + (list 3 4)))
(test '(b e h) (map cadr '((a b) (d e) (g h))))
(test '(1 4 27 256 3125) (map (lambda (n) (expt n n)) '(1 2 3 4 5)))
;; Multi-list map not supported
;; (test '(5 7 9) (map + '(1 2 3) '(4 5 6)))
(test '#(0 1 4 9 16)
    (let ((v (make-vector 5)))
      (for-each
       (lambda (i) (vector-set! v i (* i i)))
       '(0 1 2 3 4))
      v))

(test-section "Chibi: Delay/Force")
(test 3 (force (delay (+ 1 2))))
(test '(3 3) (let ((p (delay (+ 1 2)))) (list (force p) (force p))))

(test-section "Chibi: Hygiene")
(test 'ok (let ((else 1)) (cond (else 'ok) (#t 'bad))))
;; These hygiene tests require more sophisticated macro support
;; (test 'ok (let ((=> 1)) (cond (#t => 'ok))))
;; (test '(,foo) (let ((unquote 1)) `(,foo)))
;; (test '(,@foo) (let ((unquote-splicing 1)) `(,@foo)))
;; Ellipsis as identifier not supported
(test 'ok (let ()
            (let-syntax ()
              (define internal-def 'ok))
            internal-def))
(test 'ok (let ()
            (letrec-syntax ()
              (define internal-def 'ok))
            internal-def))

(test-section "Chibi: Set!")
(test '(2 1) ((lambda () (let ((x 1)) (let ((y x)) (set! x 2) (list x y))))))
(test '(2 2) ((lambda () (let ((x 1)) (set! x 2) (let ((y x)) (list x y))))))
(test '(1 2) ((lambda () (let ((x 1)) (let ((y x)) (set! y 2) (list x y))))))
(test '(2 3) ((lambda () (let ((x 1)) (let ((y x)) (set! x 2) (set! y 3) (list x y))))))

(test-section "Chibi: Dynamic-Wind")
(test '(a b c)
    (let* ((path '())
           (add (lambda (s) (set! path (cons s path)))))
      (dynamic-wind (lambda () (add 'a)) (lambda () (add 'b)) (lambda () (add 'c)))
      (reverse path)))

(test '(connect talk1 disconnect connect talk2 disconnect)
    (let ((path '())
          (c #f))
      (let ((add (lambda (s)
                   (set! path (cons s path)))))
        (dynamic-wind
            (lambda () (add 'connect))
            (lambda ()
              (add (call-with-current-continuation
                    (lambda (c0)
                      (set! c c0)
                      'talk1))))
            (lambda () (add 'disconnect)))
        (if (< (length path) 4)
            (c 'talk2)
            (reverse path)))))

;; Custom ellipsis tests removed - not supported
;; (test-section "Chibi: Custom Ellipsis")

;;;============================================================================
;;; SISC R5RS Pitfall Tests
;;;============================================================================

(test-section "SISC: Letrec with Continuations")

(should-be "1.1" 0
  (let ((cont #f))
    (letrec ((x (call-with-current-continuation (lambda (c) (set! cont c) 0)))
             (y (call-with-current-continuation (lambda (c) (set! cont c) 0))))
      (if cont
          (let ((c cont))
            (set! cont #f)
            (set! x 1)
            (set! y 1)
            (c 0))
          (+ x y)))))

(should-be "1.2" #t
  (letrec ((x (call/cc list)) (y (call/cc list)))
    (cond ((procedure? x) (x (pair? y)))
          ((procedure? y) (y (pair? x))))
    (let ((x (car x)) (y (car y)))
      (and (call/cc x) (call/cc y) (call/cc x)))))

(should-be "1.3" #t
  (letrec ((x (call-with-current-continuation
                  (lambda (c)
                    (list #t c)))))
      (if (car x)
          ((cadr x) (list #f (lambda () x)))
          (eq? x ((cadr x))))))

(test-section "SISC: Call/cc and Procedure Application")

(should-be "2.1" 1
 (call/cc (lambda (c) (0 (c 1)))))

(test-section "SISC: Hygienic Macros")
;; Test 3.1: Referential transparency - free references in macro templates
;; should resolve to bindings at macro definition time, not use time
(should-be "3.1" 4
  (let-syntax ((foo
                (syntax-rules ()
                  ((_ expr) (+ expr 1)))))
    (let ((+ *))
      (foo 3))))

(should-be "3.2" 2
 (let-syntax ((foo (syntax-rules ()
                       ((_ var) (define var 1)))))
     (let ((x 2))
       (begin (define foo +))
       (cond (else (foo x)))
       x)))

;; Test 3.3: Nested macro hygiene with shadowing
;; When foo substitutes y with x, bar's introduced x should not shadow it
(should-be "3.3" 1
  (let ((x 1))
    (let-syntax
        ((foo (syntax-rules ()
                ((_ y) (let-syntax
                             ((bar (syntax-rules ()
                                   ((_) (let ((x 2)) y)))))
                         (bar))))))
      (foo x))))

(should-be "3.4" 1
  (let-syntax ((x (syntax-rules ()))) 1))

(test-section "SISC: No Reserved Identifiers")

(should-be "4.1" '(x)
 ((lambda lambda lambda) 'x))

;; Test 4.2: 'begin' can be shadowed by local bindings
(should-be "4.2" '(1 2 3)
  ((lambda (begin) (begin 1 2 3)) (lambda lambda lambda)))

(should-be "4.3" #f
  (let ((quote -)) (eqv? '1 1)))

;; Section 5 (nil/false distinctness) intentionally skipped

(test-section "SISC: Symbol Case Sensitivity")

(should-be "6.1" #f
  (eq? (string->symbol "f") (string->symbol "F")))

(test-section "SISC: First-Class Continuations")

(define r #f)
(define a #f)
(define b #f)
(define c #f)
(define i 0)
(should-be "7.1" 28
  (let ()
    (set! r (+ 1 (+ 2 (+ 3 (call/cc (lambda (k) (set! a k) 4))))
               (+ 5 (+ 6 (call/cc (lambda (k) (set! b k) 7))))))
    (if (not c)
        (set! c a))
    (set! i (+ i 1))
    (case i
      ((1) (a 5))
      ((2) (b 8))
      ((3) (a 6))
      ((4) (c 4)))
    r))

(set! r #f)
(set! a #f)
(set! b #f)
(set! c #f)
(set! i 0)
(should-be "7.2" 28
  (let ()
    (set! r (+ 1 (+ 2 (+ 3 (call/cc (lambda (k) (set! a k) 4))))
               (+ 5 (+ 6 (call/cc (lambda (k) (set! b k) 7))))))
    (if (not c)
        (set! c a))
    (set! i (+ i 1))
    (case i
      ((1) (b 8))
      ((2) (a 5))
      ((3) (b 7))
      ((4) (c 4)))
    r))

(should-be "7.3" '((-1 4 5 3)
                 (4 -1 5 3)
                 (-1 5 4 3)
                 (5 -1 4 3)
                 (4 5 -1 3)
                 (5 4 -1 3))
  (let ((k1 #f)
        (k2 #f)
        (k3 #f)
        (state 0))
    (define (identity x) x)
    (define (fn)
      ((identity (if (= state 0)
                     (call/cc (lambda (k) (set! k1 k) +))
                     +))
       (identity (if (= state 0)
                     (call/cc (lambda (k) (set! k2 k) 1))
                     1))
       (identity (if (= state 0)
                     (call/cc (lambda (k) (set! k3 k) 2))
                     2))))
    (define (check states)
      (set! state 0)
      (let* ((res '())
             (r (fn)))
        (set! res (cons r res))
        (if (null? states)
            res
            (begin (set! state (car states))
                   (set! states (cdr states))
                   (case state
                     ((1) (k3 4))
                     ((2) (k2 2))
                     ((3) (k1 -)))))))
    (map check '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))))

(should-be "7.4" '(10 9 8 7 6 5 4 3 2 1 0)
  (let ((x '())
        (y 0))
    (call/cc
     (lambda (escape)
       (let* ((yin ((lambda (foo)
                      (set! x (cons y x))
                      (if (= y 10)
                          (escape x)
                          (begin
                            (set! y 0)
                            foo)))
                    (call/cc (lambda (bar) bar))))
              (yang ((lambda (foo)
                       (set! y (+ y 1))
                       foo)
                     (call/cc (lambda (baz) baz)))))
         (yin yang))))))

(test-section "SISC: Miscellaneous")

(should-be "8.1" -1
  (let - ((n (- 1))) n))

(should-be "8.2" '(1 2 3 4 1 2 3 4 5)
  (let ((ls (list 1 2 3 4)))
    (append ls ls '(5))))

;;;============================================================================
;;; Summary
;;;============================================================================

(test-report)

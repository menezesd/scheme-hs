;; Numeric Tower Test Suite

(display "=== R5RS Numeric Tower Tests ===\n\n")

;; Integer tests
(display "--- Integers ---\n")
(display "42 = ") (display 42) (newline)
(display "-17 = ") (display -17) (newline)
(display "#xFF = ") (display #xFF) (newline)
(display "#b1010 = ") (display #b1010) (newline)
(display "#o77 = ") (display #o77) (newline)

;; Rational tests
(display "\n--- Rationals ---\n")
(display "1/2 = ") (display 1/2) (newline)
(display "3/4 = ") (display 3/4) (newline)
(display "6/4 simplifies to: ") (display 6/4) (newline)
(display "(+ 1/2 1/3) = ") (display (+ 1/2 1/3)) (newline)
(display "(* 2/3 3/4) = ") (display (* 2/3 3/4)) (newline)

;; Float tests
(display "\n--- Floats ---\n")
(display "3.14 = ") (display 3.14) (newline)
(display "1.5e10 = ") (display 1.5e10) (newline)
(display "(+ 1.5 2.5) = ") (display (+ 1.5 2.5)) (newline)

;; Complex tests
(display "\n--- Complex ---\n")
(display "(make-rectangular 3 4) = ") (display (make-rectangular 3 4)) (newline)
(display "(real-part 3+4i) = ") (display (real-part (make-rectangular 3 4))) (newline)
(display "(imag-part 3+4i) = ") (display (imag-part (make-rectangular 3 4))) (newline)
(display "(magnitude 3+4i) = ") (display (magnitude (make-rectangular 3 4))) (newline)
(display "(sqrt -1) = ") (display (sqrt -1)) (newline)

;; Type predicates
(display "\n--- Type Predicates ---\n")
(display "(number? 42) = ") (display (number? 42)) (newline)
(display "(integer? 42) = ") (display (integer? 42)) (newline)
(display "(integer? 3.0) = ") (display (integer? 3.0)) (newline)
(display "(rational? 1/2) = ") (display (rational? 1/2)) (newline)
(display "(real? 3.14) = ") (display (real? 3.14)) (newline)
(display "(complex? 42) = ") (display (complex? 42)) (newline)

;; Exactness
(display "\n--- Exactness ---\n")
(display "(exact? 42) = ") (display (exact? 42)) (newline)
(display "(exact? 1/2) = ") (display (exact? 1/2)) (newline)
(display "(inexact? 3.14) = ") (display (inexact? 3.14)) (newline)
(display "(exact->inexact 42) = ") (display (exact->inexact 42)) (newline)
(display "(inexact->exact 3.0) = ") (display (inexact->exact 3.0)) (newline)

;; Mixed arithmetic (type promotion)
(display "\n--- Mixed Arithmetic ---\n")
(display "(+ 1 1/2) = ") (display (+ 1 1/2)) (newline)
(display "(+ 1 1.5) = ") (display (+ 1 1.5)) (newline)
(display "(+ 1/2 0.5) = ") (display (+ 1/2 0.5)) (newline)
(display "(* 2 1/3) = ") (display (* 2 1/3)) (newline)
(display "(/ 1 3) = ") (display (/ 1 3)) (newline)

;; Transcendental functions
(display "\n--- Transcendental Functions ---\n")
(display "(sin 0) = ") (display (sin 0)) (newline)
(display "(cos 0) = ") (display (cos 0)) (newline)
(display "(exp 1) = ") (display (exp 1)) (newline)
(display "(log 2.718281828) = ") (display (log 2.718281828)) (newline)
(display "(sqrt 2) = ") (display (sqrt 2)) (newline)
(display "(expt 2 10) = ") (display (expt 2 10)) (newline)
(display "(expt 2 -3) = ") (display (expt 2 -3)) (newline)

;; Rounding
(display "\n--- Rounding ---\n")
(display "(floor 3.7) = ") (display (floor 3.7)) (newline)
(display "(ceiling 3.2) = ") (display (ceiling 3.2)) (newline)
(display "(truncate -3.7) = ") (display (truncate -3.7)) (newline)
(display "(round 3.5) = ") (display (round 3.5)) (newline)

;; Rational components
(display "\n--- Rational Components ---\n")
(display "(numerator 3/4) = ") (display (numerator 3/4)) (newline)
(display "(denominator 3/4) = ") (display (denominator 3/4)) (newline)

;; Comparison
(display "\n--- Comparison ---\n")
(display "(< 1 2 3) = ") (display (< 1 2 3)) (newline)
(display "(= 1 1.0) = ") (display (= 1 1.0)) (newline)
(display "(>= 5 3 1) = ") (display (>= 5 3 1)) (newline)

(display "\n=== All numeric tower tests completed! ===\n")

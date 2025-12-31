# scheme-hs

An R5RS Scheme interpreter written in Haskell.

## Features

- **Full numeric tower**: integers, rationals, reals, and complex numbers
- **First-class functions**: lambda, closures, higher-order functions
- **Macros**: `syntax-rules` hygienic macros, `let-syntax`, `letrec-syntax`
- **Continuations**: `call/cc` for escape continuations
- **Tail call optimization**: proper TCO via trampoline
- **Mutable pairs**: `mcons`, `set-car!`, `set-cdr!` for true mutation
- **SRFI-1**: Comprehensive list library with 50+ functions
- **Standard library**: R5RS procedures including list operations, string/character handling, vectors, and I/O

### Supported Special Forms

- `define`, `set!`, `lambda`
- `if`, `cond`, `case`, `and`, `or`
- `let`, `let*`, `letrec`, named `let`
- `begin`, `do`, `delay`
- `quote`, `quasiquote`, `unquote`, `unquote-splicing`
- `define-syntax`, `let-syntax`, `letrec-syntax`, `syntax-rules`
- `call/cc`, `call-with-current-continuation`
- `call-with-input-file`, `call-with-output-file`

### Numeric Tower

```scheme
(+ 1/2 1/3)        ; => 5/6 (rationals)
(sqrt -1)          ; => 0+1i (complex)
(* 2+3i 4-5i)      ; => 23+2i
(exact? 42)        ; => #t
(inexact? 3.14)    ; => #t
```

### SRFI-1 List Library

```scheme
(iota 5)                    ; => (0 1 2 3 4)
(iota 5 10)                 ; => (10 11 12 13 14)
(take-while even? '(2 4 5 6)) ; => (2 4)
(find odd? '(2 4 5 6))      ; => 5
(delete 3 '(1 2 3 4 3))     ; => (1 2 4)
(concatenate '((1 2) (3 4))) ; => (1 2 3 4)
```

### Mutable Pairs

```scheme
(define p (mcons 1 2))
(set-car! p 10)             ; Mutates in place
(set-cdr! p 20)
p                           ; => (10 . 20)
```

## Building

Requires GHC and Cabal.

```bash
cabal build
```

## Usage

### REPL

```bash
cabal run scheme-hs
```

```scheme
scheme> (define (factorial n)
          (if (= n 0) 1 (* n (factorial (- n 1)))))
scheme> (factorial 10)
3628800
scheme> (map (lambda (x) (* x x)) '(1 2 3 4 5))
(1 4 9 16 25)
```

### Run a File

```bash
cabal run scheme-hs -- myfile.scm
```

## Running Tests

The project includes comprehensive tests using HSpec and QuickCheck:

```bash
cabal test                            # Summary output
cabal test --test-show-details=direct # Detailed output
```

Test coverage includes:
- Parser tests for all syntax
- Numeric tower operations
- Integration tests for evaluation
- Property-based tests for arithmetic identities and parsing roundtrips

## Project Structure

```
scheme-hs/
├── src/
│   ├── Main.hs        # Entry point, REPL, standard library (inc. SRFI-1)
│   ├── Types.hs       # LispVal, SchemeNum, errors, environment
│   ├── Parser.hs      # Parsec-based parser
│   ├── Eval.hs        # Evaluator with TCO, special forms, macros
│   ├── Env.hs         # Mutable environment (Data.Map-based)
│   └── Primitives.hs  # Built-in procedures
├── test/
│   ├── Main.hs           # Test runner
│   ├── ParserSpec.hs     # Parser tests
│   ├── TypesSpec.hs      # Numeric tower tests
│   └── IntegrationSpec.hs # End-to-end tests
└── scheme-hs.cabal
```

## Implementation Notes

### Tail Call Optimization

The interpreter uses a trampoline-based approach for TCO. Tail-position calls return a `TailApply` thunk that the trampoline loop evaluates, avoiding stack growth:

```scheme
;; This won't blow the stack
(define (count-down n)
  (if (= n 0) "done" (count-down (- n 1))))
(count-down 100000)  ; => "done"
```

### Environment

Variable lookups use `Data.Map.Strict` for O(log n) performance instead of association lists.

### Error Messages

Errors can include source location information via the `WithSource` wrapper for better debugging.

## Examples

### Higher-Order Functions

```scheme
(define (compose f g)
  (lambda (x) (f (g x))))

(define add1 (lambda (x) (+ x 1)))
(define square (lambda (x) (* x x)))

((compose square add1) 5)  ; => 36
```

### Continuations

```scheme
(define (find-first pred lst)
  (call/cc
    (lambda (return)
      (for-each (lambda (x)
                  (if (pred x) (return x)))
                lst)
      #f)))

(find-first even? '(1 3 5 6 7))  ; => 6
```

### Macros

```scheme
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))

(when #t
  (display "hello")
  (newline))
```

### File I/O with Auto-Close

```scheme
(call-with-output-file "output.txt"
  (lambda (port)
    (display "Hello, World!" port)))
;; Port is automatically closed, even on error
```

## License

MIT

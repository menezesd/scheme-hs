# scheme-hs

An R5RS Scheme interpreter written in Haskell.

## Features

- **Full numeric tower**: integers, rationals, reals, and complex numbers
- **First-class functions**: lambda, closures, higher-order functions
- **Macros**: `syntax-rules` hygienic macros, `let-syntax`, `letrec-syntax`
- **Continuations**: `call/cc` for escape continuations
- **Standard library**: R5RS procedures including list operations, string/character handling, vectors, and I/O

### Supported Special Forms

- `define`, `set!`, `lambda`
- `if`, `cond`, `case`, `and`, `or`
- `let`, `let*`, `letrec`, named `let`
- `begin`, `do`, `delay`
- `quote`, `quasiquote`, `unquote`, `unquote-splicing`
- `define-syntax`, `let-syntax`, `letrec-syntax`, `syntax-rules`
- `call/cc`, `call-with-current-continuation`

### Numeric Tower

```scheme
(+ 1/2 1/3)        ; => 5/6 (rationals)
(sqrt -1)          ; => 0+1i (complex)
(* 2+3i 4-5i)      ; => 23+2i
(exact? 42)        ; => #t
(inexact? 3.14)    ; => #t
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

The project includes 164 tests using HSpec and QuickCheck:

```bash
cabal test                            # Summary output
cabal test --test-show-details=direct # Detailed output
```

Test coverage includes:
- Parser tests for all syntax
- Numeric tower operations
- Integration tests for evaluation
- Property-based tests for arithmetic identities

## Project Structure

```
scheme-hs/
├── src/
│   ├── Main.hs        # Entry point, REPL, standard library
│   ├── Types.hs       # LispVal, SchemeNum, errors, environment
│   ├── Parser.hs      # Parsec-based parser
│   ├── Eval.hs        # Evaluator, special forms, macros
│   ├── Env.hs         # Mutable environment operations
│   └── Primitives.hs  # Built-in procedures
├── test/
│   ├── Main.hs           # Test runner
│   ├── ParserSpec.hs     # Parser tests
│   ├── TypesSpec.hs      # Numeric tower tests
│   └── IntegrationSpec.hs # End-to-end tests
└── scheme-hs.cabal
```

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

## License

MIT

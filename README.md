# What

Learning basic programming concepts through Scheme with Simply Scheme, then with SICP.

# Simply Scheme

`(require (planet dyoo/simply-scheme:1:2/simply-scheme))`

```scheme
word

every      ; map
keep       ; filter
accumulate ; reduce
repeated

; condition
cond
if
'()   ; = empty value

first ; = car 
butfirst (remove first character/item) (bf) ; = cdr
butlast (bl)
append
prepend-every
item ; (item 4 '(return the fourth item in this sentence))

; to check type
number?
boolean?
word?
sentence?

; other predicates
even? 
odd?
equal?
member?
before?
empty?
```

Basic list manipuation function of scheme:

```scheme
car   ; = first
cdr   ; = butfirst
cons  ; = construct a new pair / list by prepending an element to the beginning of an existing list
;; (cons '1 '2)
;; (cons '0 '(1 2 3 4))
```

# Special form syntax

## `define` and `lambda`

1. Define the global variables

```scheme
(define pi 3.14)

pi
;; 3.14
```

2. Define a procedure

```scheme
(define (sqrt x) (* x x))

(sqrt 4)
;; 16
```

3. Define anonymous function then invoke it

```scheme
((lambda (x) (* x x)) 4)
;; 16
```

4. Define function that returns another function

```scheme
(define (compose F G)
  (lambda (x) (F (G x)))
)

((compose sqrt abs) -25)
```

## `let`

Define the local variables.

```scheme
(let ((variable1 value1)
      (variable2 value2)
      ...)
  body-expressions)
```


## Predicates

Func: `if`, `cond`

```scheme
(define (switch-case-example value)
  (cond
    ((= value 1) (display "Case 1"))
    ((= value 2) (display "Case 2"))
    ((= value 3) (display "Case 3"))
    (else (display "Default case"))
  )
)

; Example usage
(switch-case-example 2) ; This will display "Case 2"

(if (number? 'hihi) #t #f)
; #f
```

## Higher order function

`every`, `keep`, `accumulate`, `repeated`

# Resources

1. [Book: Simply Scheme (online reading)](https://people.eecs.berkeley.edu/~bh/ss-toc2.html)

2. [Link to download source code of Simply Scheme book](https://people.eecs.berkeley.edu/~bh/downloads/)

3. [Differences between Scheme and Common Lisp](https://people.eecs.berkeley.edu/~bh/ssch27/appendix-cl.html)
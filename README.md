# Lisp interpreter and compiler based on Haskell

# REPL
- ```stack exec ki -- -repl```

## 1. Interpret
### Arithmetic expression
- ```(<operator> <expression> <expression>)```
- Calculation: ```+ - * /```
- Logic: ```and or not```
- Compare: ```> < >= <= =``` (Numbers only)
```
>>> (+ 1 2)
3
>>> (and True False)
False
>>> (and (or True False) True)
True
```

### Assignment statement
- ```(set! <variable> <expression>)```
```
>>> (set! x 1)
>>> x
1.0
```

### Array statement
- ```(make-vector <variable> <expression>)```
- ```(vector-set! <variable> <expression> <expression>)```
- ```(vector-ref <variable> <expression>)```
```
>>> (make-vector x 6)
>>> x
[Undefined, Undefined, Undefined, Undefined, Undefined, Undefined]
>>> (vector-set! x (+ 1 2) (+ 2 3))
>>> x
[Undefined, Undefined, Undefined, 5.0, Undefined, Undefined]
>>> (vector-ref x 3)
5.0
```

### If statement
- ```(if <expression> <statement>)```
- Expression should be logical
```
>>> (set! x 1)
>>> (if (< x 5) (set! x (+ x 1)))
>>> x
2.0
```

### While statement
- ```(while <expression> <statement>)```
- Expression should be logical
```
>>> (set! x 1)
>>> (while (< x 5) (set! x (+ x 1)))
>>> x
5.0
```

### Statement block
- ```(begin <statement> [<statement>..])```
```
>>> (begin (set! x 1.2) (set! y 3.2) (set! z (+ x y)))
>>> x
1.2
>>> y
3.2
>>> z
4.4
```

### Let expression
- ```(let <variable> <expression> <expression>)```
- Equals to (let var = expr in expr)
- Can be nested
```
>>> (let x (+ 1 2) (+ x 5))
8.0
>>> (let x 1 (let y 2 (+ x y)))
3.0
>>> (let x (lambda y (+ y 1)) (x 2))
3.0
```

### Function definition
- ```(define (<variable> [<variable>..]) <statement>)```
```
>>> (define (foo x y) (return (+ x y)))
>>> (foo 1 2)
3.0
>>> (define (bar x) (if (= x 5) (return 5) (return (bar (+ x 1)))))
>>> (bar 1)
5.0
```

### Higher-order function / lambda expression
- ```(lambda <variable> <expression>)```
- Can be nested
```
>>> (set! x (lambda y (+ y 1)))
>>> x
["y"] Return (+ y 1.0) fromList []
>>> (x 1)
2.0
>>> (set! foo (lambda x (lambda y (+ x y))))
>>> foo
["x"] (return (lambda y (+ x y))) fromList []
>>> (foo 2)
["y"] (return (+ x y)) fromList [("x",DoubleValue 2.0)]
>>> ((foo 2) 3)
5.0
```

## 2. Pretty-printer
- Using ```:t``` to prettify the most recent valid input

### Arithmetic expression
```
>>> (+ 1 2) 
3.0
>>> :t
(+ 1.0 2.0)
>>> (+ (- 1 2) 1)
0.0
>>> :t
(+ (- 1.0 2.0)
   1.0)
```

### If expression
```
>>> (if (> 1 2) skip (set! x 2)) 
>>> :t
(if (> 1.0 2.0)
    skip
  (set! x
        2.0))
```

### While expression
```
>>> (while (> 1 2) (set! y 1)) 
>>> :t
(while (> 1.0 2.0)
  (set! y 
        1.0))
```

### Let expression
```
>>> (let y (+ 1 x) (+ x y)) 
5.0
>>> :t
(let y
  (+ 1.0 x)
  (+ x y))
```

### Function definition
```
>>> (define (foo x) (return (+ 1 x))) 
>>> :t
(define foo (x)
  (return (+ 1.0 x))
>>> (foo 1)
2.0
```

# Interpreter
- ```stack exec ki -- -t <inFile> -o <outFile>```
- ```stack exec ki -- -t <inFile> -o <outFile>```
```
# Output AST
stack exec kc -- -t /Users/.../in.txt -o /Users/.../out.txt
# Interpreting executate
stack exec kc -- -i /Users/.../in.txt -o /Users/.../out.txt
```

# Compiler
- ```stack exec kc -- <inFile> -o <outFile>```
- ```stack exec kc -- <outFile> -ir```
```
# Compile to IR
stack exec kc -- /Users/.../in.txt -o /Users/.../out.txt 
# Executate IR
stack exec kc -- /Users/.../out.txt -ir
```

# Example
- KMP algorithm

```
(define (kmp text pattern m n)
  (begin
    (set! i 0)
    (set! j (- 0 1))
    (set! a 0)
    (make-vector answer 100)
    (make-vector next 100)
    (vector-set! next 0 (- 0 1))
    (while (< i n)
      (begin
        (while (and (> j (- 0 1)) (not (= (vector-ref pattern i) (vector-ref pattern j))))
          (set! j (vector-ref next j))
        )
        (set! i (+ 1 i))
        (set! j (+ 1 j))
        (if (and (and (not (= i n)) (not (= j m))) (= (vector-ref pattern i) (vector-ref pattern j)))
          (vector-set! next i (vector-ref next j))
          (if (and (= i n) (= j m))
            (vector-set! next i (vector-ref next j))
            (vector-set! next i j)
          )
        )
      )
    )
    (set! i 0)
    (set! j 0)
    (while (< j m)
      (begin
        (while (and (> i (- 1 0)) (not (= (vector-ref text j) (vector-ref pattern i))))
          (set! i (vector-ref next i))
        )
        (set! i (+ 1 i))
        (set! j (+ 1 j))
        (if (>= i n)
          (begin
            (vector-set! answer a (- j i))
            (set! a (+ 1 a))
            (set! i (vector-ref next i))
          )
        )
      )
    )
    (return answer)
  )
)
(define (main)
  (begin
    (make-vector text 8)
    (make-vector pattern 2)
    (vector-set! text 0 'a')
    (vector-set! text 1 'n')
    (vector-set! text 2 'p')
    (vector-set! text 3 'a')
    (vector-set! text 4 'n')
    (vector-set! text 5 'm')
    (vector-set! text 6 'a')
    (vector-set! text 7 'n')
    (vector-set! pattern 0 'a')
    (vector-set! pattern 0 'n')
    (return (kmp text pattern 8 2))
  )
)
```

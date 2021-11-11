
    <module> := #lang ‹langname› ‹topform›*
    <topform> := <expr> | <definition>
    <definition> := ( define ‹id› ‹expr› )               ; bind val
                                    |  ( define ( ‹id› ‹id›* ) ‹expr›+ )    ; bind proc
    
    <expr> := 
       ; functions
           ( ‹id› ‹expr›* )                  ; proc call
       | (<func-expr> <expr>* )            ; proc call
       | ( lambda ( ‹id›* ) ‹expr›+ )      ; lambda

       ; branching
       |  ( if <expr> <expr> <expr> )                
       |  ( and ‹expr›* ) | ( or ‹expr›* )
         ( cond {[ ‹expr› ‹expr›* ]}* )  ; like switch

        ; local binding
        ; bind id to expr, ret lst
       ( let ( {[ ‹id› ‹expr› ]}* ) ‹expr›+ )
       ; later binding can use early ids
       ( let* ( {[ ‹id› ‹expr› ]}* ) ‹expr›+ )
       ; bindings available to all other exprs
       ( letrec ( {[ ‹id› ‹expr› ]}* ) ‹expr›+ )
       (set! id expr)


```lisp

; list

(list 1 2 3 4 5)     ; create list

empty                ; '()
(cons fst-elem rst-part) 
(define lst (list 1 2 3 4 5))

(first lst)          ; head
(rest lst)           ; tail

(length lst)         ; 5
(list-ref lst 0)     ; 1, indexing
(append lst lst)     
(reverse lst)

; tests
(member 9 lst)       ; #f test constains
(empty? lst)         ; #f
(cons? lst)          ; #t constructor test


(map func lst)
(andmap test-func lst) ; test all
(ormap test-func lst)  ; test any

(filter test-func lst)
(foldl func init lst)


; pair

(cons 1 2)   ; create a pair, overload, '(1 . 2)


(car some-pair) ; take the 1st
(cdr some-pair) ; take the 2nd



; infix notation
(1 . < . 2)          ; (< 1 2)
```


The syntax of Racket is not defined directly in terms of character streams. Instead, the syntax is determined by two layers:

-   a **reader layer**, which turns a sequence of characters into lists, symbols, and other constants. _The rules for printing and reading go together_. And
    
-   an **expander layer**, which processes the lists, symbols, and other constants to parse them as an expression.

# Data type
```lisp
; boolean
#t
#f
(if "no" 1 0)  ; 1

; number
#e0.5 ; exact number 1/2
#i0.5 ; inexact

#b102102 ; binary
#x03bb   ; hexadecimal

exact->inexact
inexact->exact
integer?  
complex?
=
equiv?

;char
#\λ
#\u03BB
"λ"   ; string
(string-ref "Apple" 0) ; #\A

; #"Apple" ; byte string

; (byte-ref #"Apple", 0) ; 65
(make-bytes 3 65) ; #"AAA"


; symbol

(quote some-id)      ; create a symbol as 'some-id 
'(1 2 3)             ; abbrev for quote
(gensym)             ; generate fresh uninterned symbols
(quote (1 2 3 4 5))  
(quote (1 . 2)) 
(symbol? 'a); #t


; keyword
(eq? '#:apple (string->keyword "apple")); #t

; hash table
(define ht (make-hash))                ; new hash table
(hash-set! ht "apple" '(red round))    ; add elem
(hash-set! ht "banana" '(yellow long)) ; add elem
(hash-ref ht "apple")                  ; get
(hash-ref ht "coconut")                ; error
(hash-ref ht "coconut" "not there")    ; get with default val

; literal hash table
#hash(("apple" . red)
      ("banana" . yellow))



; side effects, non pure function
(begin expr ...+)   ; runs all returns last
(begin0 expr ...+)  ; runs all returns first
(when test-expr then-body ...+)   ; if without else
(unless test-expr then-body ...+)
```
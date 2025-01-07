;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname bexp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 07 Problem 2: "bexp.rkt"
;; ***************************************************

;; Q2: "bexp.rkt"

;; a)
;; Data def for Boolean
;; A Boolean Ex is:
;; * (anyof 0 1)

;; A OpNode is a:
;; * (list (anyof 'AND 'OR 'XOR) (listof BExp))

;; A BExp is a
;; * Boolean
;; * OpNode

;; Template for OpNode:
;; (opnode-template OpNode) Consumes a OpNode and runs through a different function depending on
;; the OpNode that it is given.
;; opnode-template: OpNode -> Any
(define (opnode-template OpNode)
  (cond
    [(symbol=? 'AND (first OpNode)) ...]
    [(symbol=? 'OR (first OpNode)) ...]
    [(symbol=? 'XOR (first OpNode)) ...]))

;; (bexp-template OpNode) Consumes a BExp and runs through a different function depending on
;; the BExp that it is given.
;; bexp-template: BExp -> Any
;; Template for BExp:
(define (bexp-template BExp)
  (cond
    [(or (= 1 BExp) (= 2 BExp)) ...]
    [(cons? BExp) (opnode-template BExp)]))

;; b)
;; (eval list) consumes a BExp and produces the Boolean value of the BExp
;; Example:
(check-expect (eval (list 'AND (list 1 1 1))) 1)
;; eval: BExp -> Boolean
(define (eval list)
  (cond
    [(empty? list) 0]
    [(symbol=? (first list) 'AND) (my_and (second list))]
    [(symbol=? (first list) 'OR) (my_or (second list))]
    [(symbol=? (first list) 'XOR) (my_xor (second list))]
    [else (first list)]))
;; (my_and list) consumes a (listof Boolean) and produces the Boolean of an 'AND quantifier.
;; Example:
(check-expect (my_and (list 1 1 1)) 1)
;; my_and: (listof Boolean) -> Boolean
(define (my_and list)
  (cond
    [(empty? list) 1]
    [(cons? (first list)) (eval (append (first list) (rest list)))]
    [(= 1 (first list)) (my_and (rest list))]
    [else 0]))
;; (my_or list) consumes a (listof Boolean) and produces the Boolean of an 'OR quantifier.
;; Example:
(check-expect (my_or (list 1 0 0)) 1)
;; my_or: (listof Boolean) -> Boolean
(define (my_or list)
  (cond
    [(empty? list) 0]
    [(cons? (first list)) (eval (append (first list) (rest list)))]
    [(= 1 (first list)) 1]
    [else (my_or (rest list))]))

;; (my_xor list) consumes a (listof Boolean) and produces the Boolean of an 'XOR quantifier.
;; Example:
(check-expect (my_xor (list 1 1 0)) 0)
;; my_xor: (listof Boolean) -> Boolean
(define (my_xor list)
  (cond
    [(empty? list) 0]
    [(cons? (first list)) (eval (append (first list) (rest list)))]
    [(= 0 (first list)) (my_xor (rest list))]
    [(= 1 (my_or (rest list))) 0]
    [else 1]))

;; Tests:
;; Created by me:
(check-expect (eval (list 'AND (list 0 1 1))) 0)
(check-expect (eval (list 'OR (list 1 1 0))) 1)
(check-expect (eval (list 'OR (list 1 1 1))) 1)
(check-expect (eval (list 'OR (list 0 0 0))) 0)
(check-expect (eval (list 'XOR (list 1 1 0))) 0)
(check-expect (eval (list 'XOR (list 1 0 0))) 1)
(check-expect (eval (list 'XOR (list 0 0 0))) 0)
(check-expect (eval (list 'AND (list 0 0 0))) 0)
;; Given in assignment:
(check-expect (eval (list 'AND (list 0 1 1))) 0)
(check-expect (eval (list 'OR (list 0 1 1))) 1)
(check-expect (eval (list 'XOR (list 0 1 1))) 0)
(check-expect (eval (list 'AND (list 1 (list 'XOR (list 0 1 (list 'OR (list 0 0 0)))) 1))) 1)

;; c)
;; (bidexp->string list) consumes a BExp and produces the same BExp in string form.
;; Example:
(check-expect (bidexp->string (list 'AND (list 1 1 1))) "(t*t*t)")
;; bidexp->string: BExp -> Str
(define (bidexp->string list)
  (cond
    [(symbol=? (first list) 'AND) (bidAND (second list) empty)]
    [(symbol=? (first list) 'OR) (bidOR (second list) empty)]
    [else (bidXOR (second list) empty)]))

;; (construct_string acc) consumes a list and produces the same list with the final item replaced
;; with #\)
;; Example:
(check-expect (construct_string (list #\t #\* #\f #\*)) (list #\t #\* #\f #\)))
;; construct_string: List -> List
(define (construct_string acc)
  (cond
    [(empty? (rest acc))
     (opporater (first acc))]
    [(char=? (first acc) #\)) (cons #\) (construct_string (rest acc)))]
    [else (cons (first acc) (construct_string (rest acc)))]))

;; (symbol->char sym) Consumes a simple of length 1 and produces the Symbol's Char.
(check-expect (symbol->char 'a) #\a)
;; symbol->char: Sym -> Char
(define (symbol->char sym)
  (cond
    [(= (string-length (symbol->string sym)) 1)
     (first (string->list (symbol->string sym)))]))

;; (opporater char) consumes a Char and produces a list with one item: #\)
;; Example:
(check-expect (opporater #\*) (cons #\) empty))
;; opporater: Char -> (list Char)
;; Requires: char = #|* or #\+ or #\.
(define (opporater char)
  (cond
    [(char=? char #\*) (cons #\) empty)]
    [(char=? char #\+) (cons #\) empty)]
    [else (cons #\) empty)]))

;; (bidAND lst acc) consumes a (listof Boolean) and an empty list and produces the string for
;; if 'AND was applied.
;; Example:
(check-expect (bidAND (list 1 0 0 1) empty) "(t*f*f*t)")
;; bidAND: List empty -> Str
(define (bidAND lst acc)
  (cond
    [(empty? lst) (list->string (append (list #\() (construct_string acc)))]
    [(cons? (first lst))
     (bidAND (rest lst)
             (append acc (string->list (bidexp->string (first lst))) (list #\*)))]
    [(symbol? (first lst))
     (bidAND (rest lst)
             (append acc (list #\' (symbol->char(first lst)) #\*)))]
    [(= 1 (first lst)) (bidAND (rest lst) (append acc (list #\t #\*)))]
    [(= 0 (first lst)) (bidAND (rest lst) (append acc (list #\f #\*)))]))

;; (bidOR lst acc) consumes a (listof Boolean) and an empty list and produces the string for
;; if 'OR was applied.
;; Example:
(check-expect (bidOR (list 1 0 0 1) empty) "(t+f+f+t)")
;; bidOR: List empty -> Str
(define (bidOR lst acc)
  (cond
    [(empty? lst) (list->string (append (list #\() (construct_string acc)))]
    [(cons? (first lst))
     (bidOR (rest lst)
            (append acc (string->list (bidexp->string (first lst))) (list #\+)))]
    [(symbol? (first lst))
     (bidOR (rest lst)
            (append acc (list #\' (symbol->char (first lst)) #\+)))]
    [(= 1 (first lst)) (bidOR (rest lst) (append acc (list #\t #\+)))]
    [(= 0 (first lst)) (bidOR (rest lst) (append acc (list #\f #\+)))]))

;; (bidAND lst acc) consumes a (listof Boolean) and an empty list and produces the string for
;; if 'XOR was applied.
;; Example:
(check-expect (bidXOR (list 1 0 0 1) empty) "(t.f.f.t)")
;; bidXOR: List empty -> Str
(define (bidXOR lst acc)
  (cond
    [(empty? lst) (list->string (append (list #\() (construct_string acc)))]
    [(cons? (first lst))
     (bidXOR (rest lst)
             (append acc (string->list (bidexp->string (first lst))) (list #\.)))]
    [(symbol? (first lst))
     (bidXOR (rest lst)
             (append acc (list #\' (symbol->char (first lst)) #\.)))]
    [(= 1 (first lst)) (bidXOR (rest lst) (append acc (list #\t #\.)))]
    [(= 0 (first lst)) (bidXOR (rest lst) (append acc (list #\f #\.)))]))

;; Tests:

;; Created by me:
(check-expect (bidexp->string (list 'AND (list 0 1 1))) "(f*t*t)")
(check-expect (bidexp->string (list 'OR (list 1 1 0))) "(t+t+f)")
(check-expect (bidexp->string (list 'OR (list 1 1 1))) "(t+t+t)")
(check-expect (bidexp->string (list 'OR (list 0 0 0))) "(f+f+f)")
(check-expect (bidexp->string (list 'XOR (list 1 1 0))) "(t.t.f)")
(check-expect (bidexp->string (list 'XOR (list 1 0 0))) "(t.f.f)")
(check-expect (bidexp->string (list 'XOR (list 0 0 0))) "(f.f.f)")
;; Given in assignment:
(check-expect (bidexp->string (list 'AND (list 0 1 1))) "(f*t*t)")
(check-expect (bidexp->string (list 'OR (list 0 1 1))) "(f+t+t)")
(check-expect (bidexp->string (list 'XOR (list 0 1 1))) "(f.t.t)")

(check-expect (bidexp->string (list 'AND (list 1 (list 'XOR (list 0 1 (list 'OR (list 0 0 0)))) 1)))
              "(t*(f.t.(f+f+f))*t)")
(check-expect (bidexp->string (list 'AND (list 1 (list 'AND (list 0 1)))))
              "(t*(f*t))")
(check-expect (bidexp->string (list 'XOR (list 0 't 'u 1 'w)))
              "(f.'t.'u.t.'w)")
(check-expect (bidexp->string (list 'OR (list 0 't 'u 1 'w)))
              "(f+'t+'u+t+'w)")
(check-expect (bidexp->string (list 'AND (list 0 't 'u 1 'w)))
              "(f*'t*'u*t*'w)")
(check-expect (bidexp->string (list 'AND (list 1 (list 'OR (list 0 (list 'XOR (list 0 1 0)))))))
              "(t*(f+(f.t.f)))")

;; d)

;; (eval-id list id) consumes a BIDExp and a (listof (list Sym (1 or 0))) and yields the 1 if the
;; BIDExp is true and false otherwise.
;; Example:
(check-expect (eval-id (list 'AND (list 'x 'y 0)) identifier-table) 0)
;; eval-id: BIDExp List -> Num
(define (eval-id lst id)
  (eval (list (first lst) (simplify_expression (second lst) id))))

;; (simplify_expression list id) consumes a (listof Boolean Sym) and a (listof (list Sym Boolean)
;; and produces the same list with any Sym replaced with it's associated Boolean.
;; Example:
(check-expect (simplify_expression (list 1 0 'x 'y) (list (list 'x 1) (list 'y 0)))
              (list 1 0 1 0))
;; simplify_expression: (listof Boolean Sym) (listof (list Sym Boolean)) -> (listof Boolean)
(define (simplify_expression list id)
  (cond
    [(empty? list) empty]
    [(number? (first list)) (cons (first list) (simplify_expression (rest list) id))]
    [else (cons (symbol->identifier (first list) id)
                #| symbol -> identifier |# (simplify_expression (rest list) id))]))

;; (symbol->identifier sym id) consumes a Sym and a (listof (list Sym Boolean)) and produces the
;; Boolean in the id that is associated with that Sym.
;; Example:
(check-expect (symbol->identifier 'z (list (list 'x 0) (list 'z 1) (list 'y 0))) 1)
;; symbol->identifier: Sym (listof (list Sym Boolean)
(define (symbol->identifier sym id)
  (cond
    [(empty? id) sym]
    [(symbol=? sym (first (first id))) (second (first id))]
    [else (symbol->identifier sym (rest id))]))

;; Tests:
(define identifier-table (list (list 'x 1) (list 'y 0)))
;; Created by me:
(check-expect (eval-id (list 'AND (list 'x 'x 1)) identifier-table) 1)
(check-expect (eval-id (list 'OR (list 'x 'y 0)) identifier-table) 1)
(check-expect (eval-id (list 'OR (list 'y 0 'y)) identifier-table) 0)
(check-expect (eval-id (list 'XOR (list 'x 'y 0)) identifier-table) 1)
(check-expect (eval-id (list 'XOR (list 'x 'y 1)) identifier-table) 0)
(check-expect (eval-id (list 'XOR (list 'y 'y 0)) identifier-table) 0)
(check-expect (eval-id (list 'XOR (list 1 (list 'AND (list 0 1 1)))) empty) 1)

;; Given in assignment:

(check-expect (eval-id (list 'AND (list 0 'x 1)) identifier-table) 0)
(check-expect (eval-id (list 'OR (list 'x 'y 1)) identifier-table) 1)
(check-expect (eval-id (list 'XOR (list 0 'y 1)) identifier-table) 1)
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname funabst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 08 Problem 2: "funabst.rkt"
;; ***************************************************

;; Q2:

;; a)
;; (or-pred predicate lst) consumes a predicate and a list and produces true if any element in the
;; consumed list produces true, and false otherwise. An empty list should provide false.
;; Example:
(check-expect (or-pred symbol? (list 1 2 3 'four 5)) true)
;; or-pred: (Any -> Bool) List -> Bool
(define (or-pred pred lst)
  (cond
    [(empty? lst) false]
    [(pred (first lst)) true]
    [else (or-pred pred (rest lst))]))
;; Tests:
(check-expect (or-pred char? (list "a" "b" "c" "d")) false)
(check-expect (or-pred char? (list "a" "b" "c" #\d)) true)
(check-expect (or-pred cons? (list empty)) false)
(check-expect (or-pred list? (list empty)) true)
;; Given:
(check-expect (or-pred even? empty) false)
(check-expect (or-pred odd? (list 6 10 4)) false)
(check-expect (or-pred string? (list 5 "wow")) true)


;; b)
;; (map2argfn lofun lo2n) consumes a (listof Function) and a (list Num Num) and produces the list
;; of the results of applying each function in turn to the given two numbers.
;; Requires:
;; All functions consume two numbers as arguments.
;; Example:
(check-expect (map2argfn (list <= < > - +) (list 5 5))
              (list true false false 0 10))
;; map2argfn: (listof (Num Num -> Any)) (list Num Num) -> (listof Any)
(define (map2argfn lofun lo2n)
  (cond
    [(empty? lofun) empty]
    [else (cons ((first lofun) (first lo2n) (second lo2n)) (map2argfn (rest lofun) lo2n))]))
;; Tests:
(check-expect (map2argfn (list + - * expt) (list 4 2))
              (list 6 2 8 16))
;; Given:
(check-expect (map2argfn (list + - * / list) (list 3 2))
              (list 5 1 6 1.5 (list 3 2)))


;; c)
;; (arranged? pf-bro loa) consumes a (list predicate-function binary-relational-operator) and a
;; (listof Any) and first checks if any elements in the list produce false from the predicate function
;; which produces false. If not, the binary-relational-opperator (bro) is applied to the function.

;; A bro is used to compare two consecutive elements in the list. It produces true if elements are
;; in the desired order and false otherwise.
;; Example:
(check-expect (arranged? (list char? char<?) (list "b" "f" "q" "t")) false)
;; arranged?: (list (Any -> Bool) (X -> Bool)) (listof Any) -> Bool
;; requires: if binary-relational-operator is applied on any
;;           elements, then predicate-function produces true on
;;           elements of type X
(define (arranged? pf-bro lst)
  (cond
    [(empty? lst) true]
    [(empty? (rest lst)) ((first pf-bro) (first lst))]
    [else
     (and 
      ((first pf-bro) (first lst))
      ((first pf-bro) (second lst))
      ((second pf-bro) (first lst) (second lst))
      (arranged? pf-bro (rest lst)))]))

;; Tests:
(check-expect (arranged? (list char? char<?) (list #\b #\f #\q #\t)) true)
(check-expect (arranged? (list number? >=) (list 5 5 4 -5)) true)
(check-expect (arranged? (list number? <) (list 'the 'quick "BLUE" 'fox)) false)
;; Given:
(check-expect (arranged? (list string? >) (list "wow" 'red)) false)
(check-expect (arranged? (list string? string>?)
                         (list "wow" "cs135" "amazing")) true)
(check-expect (arranged? (list number? <) (list 1 5 3)) false)
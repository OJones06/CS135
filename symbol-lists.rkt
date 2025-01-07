;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname symbol-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 05, Problem 3 "symbol-lists.rkt"
;; ***************************************************


;; (list-of-x-symbols num symbol) consumes a natural number (num) and a symbol and produces
;; a list of size num with the symbol as its objects.
;; Example:
(check-expect (list-of-x-symbols 5 'X) (list 'X 'X 'X 'X 'X))
;; list-of-x-symbols: Num Sym -> List
(define (list-of-x-symbols num symbol)
  (cond
    [(= num 0) empty]
    [else (cons symbol (list-of-x-symbols (- num 1) symbol))]))

;; (make-symbol-lists num-list symbol) consumes a list of natural numbers and a symbol and produces
;; a nested list where inner list k has length equal to the kth natural number from the consumed
;; list and is composed only of the consumed symbol.
;; Example:
(check-expect (make-symbol-lists (list 0 0 5 0) 'Z)
              (list empty empty (list 'Z 'Z 'Z 'Z 'Z) empty))
;; make-symbol-lists: List Sym -> List
(define (make-symbol-lists num-list symbol)
  (cond
    [(empty? num-list) empty]
    [else (cons (list-of-x-symbols (first num-list) symbol)
                (make-symbol-lists (rest num-list) symbol))]))
;; Tests:
(check-expect (make-symbol-lists (list 0 2 3) 'Y)
              (list empty (list 'Y 'Y) (list 'Y 'Y 'Y)))
(check-expect (make-symbol-lists empty 'X)
              empty)
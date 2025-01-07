;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname morelistfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 06, Problem 1 "morelistfun.rkt"
;; ***************************************************

;; Q1:
;; a)
;; (my-list-ref lon index) consumes a (listof Num) and a Num and produces the element in the list
;; at the consumes index. The index of an element is a natural number representing the number
;; of elements in front of it.
;; Examples:
(check-expect (my-list-ref (list 2 3 6 7 8 9 10) 4) 8)
;; my-list-ref: (listof Num) Num -> Num
(define (my-list-ref lon index)
  (cond
    [(empty? lon) false]
    [(= 0 index) (first lon)]
    [else (my-list-ref (rest lon) (- index 1))]))
 ;; Tests:
(check-expect (my-list-ref (list 1 2 3 4 5 6 7) 6) 7)
(check-expect (my-list-ref (list 2 5 7 11) 0) 2)
(check-expect (my-list-ref (list 10 9 8 7 6) 5) false)

;; b)
;; (zip key value) consumes two lists with the same length and produces an association list
;; where keys are the elemenets of the first list and the values are the elements of the second.
;; Requires:
;; length of key = length of value
;; Example:
(check-expect (zip (list 1 2 3 4 5) (list 'a 'b 'c 'd 'e))
              (list (list 1 'a) (list 2 'b) (list 3 'c) (list 4 'd) (list 5 'e)))
;; zip: List List -> (listof List)
(define (zip key value)
  (cond
    [(empty? key) empty]
    [else (cons (list (first key) (first value)) (zip (rest key) (rest value)))]))
;; Tests:
(check-expect (zip (list "this" "that" "and" "the" "other") (list 'a 1 true false 0))
              (list (list "this" 'a) (list "that" 1) (list "and" true)
                    (list "the" false) (list "other" 0)))
(check-expect (zip (list true true false false) (list 'true 'false 'true 'false))
              (list (list true 'true) (list true 'false) (list false 'true) (list false 'false)))
(check-expect (zip empty empty) empty)

;; c)
;; (list-xor lon1 lon2) consumes two (listof Num) sorted in increasing order, and produces a sorted
;; list that contains only the items that are in a or b, but none in both.
;; Example:
(check-expect (list-xor (list 1 2 3 4 5) (list 5 6 7 8 9 10)) (list 1 2 3 4 6 7 8 9 10))
;; list-xor: (listof Num) (listof Num) -> (listof Num)
(define (list-xor lon1 lon2)
  (cond
    [(empty? lon1) lon2]
    [(empty? lon2) lon1]
    [(= (first lon1) (first lon2)) (list-xor (rest lon1) (rest lon2))]
    [(< (first lon1) (first lon2)) (cons (first lon1) (list-xor (rest lon1) lon2))]
    [else (cons (first lon2) (list-xor lon1 (rest lon2)))]))
;; Tests:
(check-expect (list-xor (list 2 3 5 7 11 13) (list 1 3 5 7 9 13 15)) (list 1 2 9 11 15))
(check-expect (list-xor (list -5 -4 -3 -2 -1 0) (list -7 -6 -5 -4)) (list -7 -6 -3 -2 -1 0))


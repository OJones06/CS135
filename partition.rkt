;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname partition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 08 Problem 3: "partition.rkt"
;; ***************************************************

;; Q3:

;; (partition pred lst) consumes a Predicate and a List and produces a two element list (list X Y)
;; where X is a list of those items in the consumed list that satisfy the predicate and Y is a list
;; of those items that don't satisfy the predicate. The order of the items is the same as originally.
;; Note: No using filter, but reverse may be used.
;; Example:
(check-expect (partition symbol? (list "a" 'b 3 'd "e" 'eff 'gee))
              (list (list 'b 'd 'eff 'gee) (list "a" 3 "e")))
;; partition: (X -> Bool) (listof X) -> (list (listof X) (listof X))
(define (partition pred lst)
  ;; (partition2 pred lst acc1 acc2) consumes a predicate, a list and two empty lists and uses
  ;; accumulative recursion to build the two lists as stated above.
  ;; (X -> Bool) (listof X) empty empty -> (listof (listof X) (listof X))
  (local [(define (partition2 pred lst acc1 acc2)
          (cond
            [(empty? lst) (list acc1 acc2)]
            [(pred (first lst)) (partition2 pred (rest lst) (append acc1 (list (first lst))) acc2)]
            [else (partition2 pred (rest lst) acc1 (append acc2 (list (first lst))))]))]
    (partition2 pred lst empty empty)))
;; Tests:
(check-expect (partition number? (list 'one "two" 3 4 5 'six "7"))
              (list (list 3 4 5) (list 'one "two" 'six "7")))
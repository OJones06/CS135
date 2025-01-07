;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tree-pred) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 08 Problem 4: "tree-pred.rkt"
;; ***************************************************

;; Q4:

(define-struct node (key left right))
;; A Node is a (make-node Nat BT BT)

;; A Binary Tree (BT) is one of:
;; * empty
;; * Node

(define my_tree (make-node 10
                           (make-node 6
                                      (make-node 4
                                                 (make-node -2 empty empty) empty)
                                      (make-node 8 empty empty))
                           (make-node 14
                                      (make-node 12 empty empty)
                                      (make-node 18
                                                 (make-node 16 empty empty)
                                                 empty))))

;; (tree-pred pred) consumes a one argument predicate and produces a function that will consume
;; a binary tree and produce true if the predicate produces true for every value and false otherwise
;; An empty tree should produce true.
;; Example:
(check-expect ((tree-pred even?) my_tree) true)
;; tree-pred: Function Node -> Bool
(define (tree-pred pred)
  ;; (this-fn tree) consumes a tree and produces a function that can determine the truth value
  ;; as stated above.
  ;; this-fn: Node -> Bool
  (local [(define (this-fn tree)
            (cond
              [(empty? tree) true]
              [(not (pred (node-key tree))) false]
              [(and (empty? (node-left tree)) (empty? (node-right tree))) true]
              [else (and (this-fn (node-left tree)) (this-fn (node-right tree)))]))] this-fn))


;; Tests:
(check-expect ((tree-pred zero?) my_tree) false)
(check-expect ((tree-pred positive?) my_tree) false)
(check-expect ((tree-pred negative?) my_tree) false)
;; Given:
(define t (make-node 5
                     (make-node 10 empty empty)
                     (make-node 15
                                (make-node 20 empty empty)
                                (make-node 33 empty empty))))
(check-expect ((tree-pred even?) t) false)
(check-expect ((tree-pred positive?) t) true)
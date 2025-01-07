;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname clique) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 10 Problem 3: "clique.rkt"
;; ***************************************************

;; Q3: clique.rkt

;; A Node is a Sym
;; A Graph is one of:
;; * empty
;; * (cons (list v (list w_1 ... w_n)) g)
;;   where g is a Graph
;;     v, w_1, ... w_n are Nodes
;;     v is the in-neighbour to w_1 ... w_n in the Graph
;;     v does not appear as an in-neighbour in g


;; (is-clique? lon g) consumes a (listof Node), and a Graph and produces true when the nodes in lon
;; form a clique in g and false otherwise. USE THE NEIGHBOURS FUNCTION AS A HELPER.
;; Example:
(check-expect (is-clique? '(A B C) '((A (B C D)) (B (A C)) (C (A B)) (D ()))) true)

;; is-clique?: (listof Node) Graph -> Bool
(define (is-clique? lon g)
  (cond
    [(empty? lon) true] ; "empty set is a clique"
    [(empty? (rest lon)) true] ; "a set containing one item is a clique"
    [else
     (local
       ;; (contains-all-nodes? node lon g) consumes a Node (listof Node) and a Graph and produces
       ;; true if
       ;; the corresponding graph item contains every element in the (listof Node) and false
       ;; otherwise.
       ;; contains-all-nodes? Node (listof Node) Graph -> Bool
       [(define (contains-all-nodes? node lon g)
          ;; filters so that the only item is the node (with its 
          (local
            [(define node-pointer (second (first (filter (lambda (x) (member? node x)) g))))
             ;; (contains? lon) runs the process needed for (contains-all-nodes?)
             ;; contains: (listof Node) -> Bool
             (define (contains? lon)
               (cond
                 [(empty? lon) true]
                 [(member? (first lon) node-pointer) (contains? (rest lon))]
                 [else false]))]
            (contains? lon)))
             
        (define ending-node (append (list (first (foldl cons empty lon)))
                                    (foldl cons empty (rest (foldl cons empty lon)))))
        ;; (clique lon) runs the process needed for any other scenario for (is-clique?) not covered
        ;; in the
        ;; initial conds
        ;; clique?: (listof Node) -> Bool
        (define (clique? lon)
          (cond
            [(not (contains-all-nodes? (first lon) (rest lon) g)) false]
            [(equal? ending-node lon) true] ; if the ending node is reached, true
            [else (clique? (append (rest lon) (list (first lon))))]))]
       (clique? lon))]))


;; Tests:
(check-expect (is-clique? '(C) '((A (B C D)) (B (A C)) (C (A B)) (D ()))) true)
(check-expect (is-clique? '() '((A (B C D)) (B (A C)) (C (A B)) (D ()))) true)
(check-expect (is-clique? '(C A D) '((A (B C D)) (B (A C)) (C (A B)) (D ()))) false)
(check-expect (is-clique? '(B A D) '((A (B C D)) (B (A C)) (C (A B)) (D (A B)))) false)
(check-expect (is-clique? '(C A D) '((A (B C D)) (B (A C)) (C (A B)) (D ()))) false)
(check-expect (is-clique? '(B A) '((A (B C D)) (B (A C)) (C (A B)) (D ()))) true)
(check-expect (is-clique? '(D A C) '((A (B C D)) (B (A C)) (C (A B)) (D ()))) false)
(check-expect (is-clique? '(D A) '((A (B C D)) (B (A C)) (C (A B)) (D ()))) false)
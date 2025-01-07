;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname spelling) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 07 Problem 3: "spelling.rkt"
;; ***************************************************

;; Q3: "spelling.rkt"

;; a)
;; A Next is a:
;; (listof Node)

;; (next-template loN) consumes a (listof Node) and deals with each Node as desired.
;; next-template: (listof Node) -> Any
(define (next-template loN)
  (cond
    [(empty? loN) ...]
    [else (... (node-template (first loN))
               (next-template (rest loN)))]))


;; A Node is a:
;; * Char
;; * Bool
;; * Next
(define-struct node (char bool next))
;; Requires:
;; Nodes in Next must be sorted in alphabetically ascending order of char.

;; (node-template node) consumes a Node and deals with each part of the node as desired.
;; node-template: Node -> Any
;; Template for Node:
(define (node-template node)
  (... (node-char node)
       (node-bool node)
       (next-template (node-next node))))
  
;; b)


;; (create-tree word_list) consumes a (listof Str) and produces a Node thay acys as the root-node
;; for the tree. Assuming each work is lower and upper case Char and the output is in Caps.
;; Example:
(check-expect (create-tree (list "OWEN" "OLIVER"))
              (make-node #\space false (list
                                        (make-node #\O false (list
                                                              (make-node #\L false (list
                                                                                    (make-node #\I false (list
                                                                                                          (make-node #\V false (list
                                                                                                                                (make-node #\E false (list
                                                                                                                                                      (make-node #\R true empty)))))))))
                                                              (make-node #\W false (list
                                                                                    (make-node #\E false (list
                                                                                                          (make-node #\N true empty))))))))))

;; create-tree: (listof Str) -> Node
(define (create-tree word_list)
  (make-tree (strings->lists (sort_strs word_list)) (make-node #\space false empty)))

;; (make-tree loloc stem) consumes a (listof (listof char)) and a stem Node and produces the
;; tree with the stem at the top.
;; Example:

(check-expect (make-tree (list (list #\O #\L #\I #\V #\E #\R) (list #\O #\W #\E #\N))
                           (make-node #\space false empty))
              (make-node #\space false (list
                                        (make-node #\O false (list
                                                              (make-node #\L false (list
                                                                                    (make-node #\I false (list
                                                                                                          (make-node #\V false (list
                                                                                                                                (make-node #\E false (list
                                                                                                                                                      (make-node #\R true empty)))))))))
                                                              (make-node #\W false (list
                                                                                    (make-node #\E false (list
                                                                                                          (make-node #\N true empty))))))))))
;; make-tree: (listof (listof char)) Node -> Node
(define (make-tree loloc stem)
  (cond
    [(empty? loloc) stem] ; last word has been created, finish recursion
    [else
     (make-tree (rest loloc)
                (build_word stem (first loloc)))]))

;; (build-word stem loc) consumes a stem Node and a (listof Char) and produces the word with the
;; stem node at the top.
;; Example:
(check-expect (build_word (make-node #\space false empty)
              (list #\O #\L #\I #\V #\E #\R))
              (make-node #\space false (list
                                        (make-node #\O false (list
                                                              (make-node #\L false (list
                                                                                    (make-node #\I false (list
                                                                                                          (make-node #\V false (list
                                                                                                                                (make-node #\E false (list
                                                                                                                                                      (make-node #\R true empty))))))))))))))
;; build_word: Node (listof Char) -> Node
(define (build_word stem loc)
  (cond
    [(empty? loc) (make-node (node-char stem) true (node-next stem))]
    [else (make-node (node-char stem)
                     (node-bool stem)
                     (add-char (node-next stem) (first loc) (rest loc)))]))

;; (add-char next char rest_list) consumes a (listof Node), a Char, and a (listof Char) and adds the
;; char as a node onto the (list Node) where appropriate.
;; Example:
(check-expect (add-char (list
                         (make-node #\L false (list
                                               (make-node #\I false (list
                                                                     (make-node #\V false (list
                                                                                           (make-node #\E false (list
                                                                                                                 (make-node #\R true empty))))))))))
                        #\M (list #\A #\P #\L #\E))
              (list
               (make-node #\L false (list
                 (make-node #\I false (list
                                       (make-node #\V false (list
                                                             (make-node #\E false (list
                                                                                   (make-node #\R true empty)))))))))
               (make-node #\M false (list
                                     (make-node #\A false (list
                                                           (make-node #\P false (list
                                                                                 (make-node #\L false (list
                                                                                                       (make-node #\E true empty)))))))))))
;; add-char: (listof Node) Char (listof Char) -> (listof Node)
(define (add-char next char rest_list)
  (cond
    [(empty? next)
     (list (build_word (make-node char false empty) rest_list))]
    [(char=? char (node-char (first next)))
     (cons (build_word (first next) rest_list) (rest next))]
    [else
     (cons (first next) (add-char (rest next) char rest_list))]))

;; (strings->lists los) consumes a (listof Str) and produces a list with nested lists of Char
;; representing the string.
;; Example:
(check-expect (strings->lists (list "OWEN" "TODD"))
              (list (list #\O #\W #\E #\N) (list #\T #\O #\D #\D)))
;; strings->lists: (listof Str) -> (listof (listof Char))
(define (strings->lists los)
  (cond
    [(empty? los) empty]
    [else (cons (string->list (string-upcase (first los))) (strings->lists (rest los)))]))

;; (sort los) puts (listof Str) in increasing order
;; Example:
(check-expect (sort_strs (list "Owen" "Ethan" "Nick" "Todd" "Carrie"))
              (list "CARRIE" "ETHAN" "NICK" "OWEN" "TODD"))
;; sort: (listof String) →(listof String)
(define (sort_strs los)
  (cond [(empty? los) empty]
        [else (insert (string-upcase (first los))
                      (sort_strs (rest los)))]))

;; (insert n slos) inserts the Str s into the sorted list slos.
;; Example:
(check-expect (insert "OWEN" (list "Carrie" "Todd" "Ethan" "Nick"))
              (list "Carrie" "OWEN" "Todd" "Ethan" "Nick"))
;; insert: Str (listof Str) -> (listof Str)
(define (insert s slos)
  (cond [(empty? slos) (cons s empty)]
        [(string<=? s (first slos)) (cons s slos)]
        [else (cons (first slos) (insert s (rest slos)))]))

;; Tests:

;; Created by me:
(check-expect (create-tree (list "Tree" "Trees" "Treat"))
              (make-node #\space false (list
                                        (make-node #\T false (list
                                                              (make-node #\R false (list
                                                                                    (make-node #\E false (list
                                                                                                          (make-node #\A false (list
                                                                                                                                (make-node #\T true empty)))
                                                                                                          (make-node #\E true (list
                                                                                                                               (make-node #\S true empty))))))))))))


;; Given in assignment:
(check-expect (create-tree (list "FEW" "FUN"))
              (make-node #\space false (list
                                        (make-node #\F false (list
                                                              (make-node #\E false (list
                                                                                    (make-node #\W true empty)))
                                                              (make-node #\U false (list
                                                                                    (make-node #\N true empty))))))))
(check-expect (create-tree (list "FUN" "FAR"))
              (make-node #\space false (list
                                        (make-node #\F false (list
                                                              (make-node #\A false (list
                                                                                    (make-node #\R true empty)))
                                                              (make-node #\U false (list
                                                                                    (make-node #\N true empty))))))))
(check-expect (create-tree (list "FUN" "fun" "fUn" "F"))
              (make-node #\space false (list
                                        (make-node #\F true (list
                                                             (make-node #\U false (list
                                                                                   (make-node #\N true empty))))))))




;; c)
;; Created by me
; (define tre (create-tree (list "Tree" "Trees" "Treat")))


(define tre (make-node #\space false (list
                                      (make-node #\T false (list
                                                            (make-node #\R false (list
                                                                                  (make-node #\E false (list
                                                                                                        (make-node #\A false (list
                                                                                                                              (make-node #\T true empty)))
                                                                                                        (make-node #\E true (list
                                                                                                                             (make-node #\S true empty))))))))))))
;; (check str tree) consumes a String and a Node and produces true if the string is in a word
;; in the tree and false otherwise regardless of capital letters.
;; Example:
(check-expect (check "tree" tre) true)
;; check: Str Node -> Bool
(define (check str tree)
  (check2 (first (strings->lists (sort_strs (list str)))) tree))

;; (check2 los tree) is a helper function that consumes a (listof Char) and a Node and produces
;; the boolean for the check function.
;; Example:
(check-expect (check2 (list #\T #\R #\E #\E) tre) true)
;; check2: (listof Char) Node -> Bool
(define (check2 loc tree)
  (cond
    [(and (empty? loc) (node-bool tree)) true] ; (1)
    [(and (empty? loc) (not (node-bool tree))) false] ; (2)
    [(empty? (node-next tree)) false] ; (3)
    [(not (char=? (first loc) (node-char (first (node-next tree)))))
     (check2 loc (make-node (node-char tree) (node-bool tree) (rest (node-next tree))))]
    [(char=? (first loc) (node-char (first (node-next tree))))
     (check2 (rest loc) (first (node-next tree)))]
    [else "this should never happen"]))

;; Tests:

(check-expect (check "TREE" tre) true)
(check-expect (check "tReEs" tre) true)
(check-expect (check "trea" tre) false)
(check-expect (check "tre" tre) false)
(check-expect (check "treat" tre) true)
(check-expect (check "T" tre) false)

;; Given in assignment:

(define dict (create-tree (list "FEW" "FUN")))

(check-expect (check "FEW" dict) true)
(check-expect (check "few" dict) true)
(check-expect (check "potatoes" dict) false)


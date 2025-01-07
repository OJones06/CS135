;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname n-queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 10 Problem 4: "n-queens.rkt"
;; ***************************************************

;; Q4: n-queens.rkt

;; A Position is a (list Nat Nat)
;;
;; A Candidate is a (listof Position)
;; Requires: No two Positions are the same

;; a)
(define p1 '(2 4))

;; (attacking? p1 p2) produces true if a queen at position p1 is attacking a queen at p2.
;; Example:
(check-expect (attacking? p1 '(0 6)) true)

;; attacking?: Position Position -> Bool
(define (attacking? p1 p2)
  (cond
    [(or (= (first p1) (first p2))
         (= (second p1) (second p2))) true] ;; same row or collumn.
    [else (local [(define slope (/ (- (second p2) (second p1))
                                   (- (first p2) (first p1))))]
            (or (= slope 1) (= slope -1)))])) ;; slope of 1 or -1 means same diagonal.
    

(check-expect (attacking? p1 '(7 4)) true)
(check-expect (attacking? p1 '(5 7)) true)
(check-expect (attacking? p1 '(5 3)) false)
(check-expect (attacking? p1 '(6 3)) false)

;; b)
(define left-soln '((3 2) (2 0) (1 3) (0 1)))
(define right-soln '((3 1) (2 3) (1 0) (0 2)))

;; (valid-cand? cand n) consumes a Candidate and a natural number and produces true if the cand is
;; a valid solution or partial solution for an N-Queens puzzle of size n.
;; Example:
(check-expect (valid-cand? left-soln 4) true)

;; valid-cand? Candidate Num -> Bool
(define (valid-cand? cand n)
  ;; (attacking-any pos cand) consumes a Position and a Candidate and produces true if the Position
  ;; is attacking any candidates and false otherwise.
  (local [(define (attacking-any? pos cand)
            (cond
              [(or (> (first pos) (- n 1)) (> (second pos) (- n 1))) true]
              [(empty? cand) false]
              [(attacking? pos (first cand)) true]
              [else (attacking-any? pos (rest cand))]))]
    (cond
      [(empty? cand) true]
      [(attacking-any? (first cand) (rest cand)) false]
      [else (valid-cand? (rest cand) n)])))

;; Tests:
(check-expect (valid-cand? '((0 0) (3 4)) 4) false)
(check-expect (valid-cand? right-soln 4) true)
(check-expect (valid-cand? '((0 0) (2 3) (3 1) (5 2))
                           7) true)
(check-expect (valid-cand? '((0 0) (2 3) (3 1) (5 1))
                           7) false)

;; c)
;; (neighbours-naive cand n) consumes a Candidate and the size of the n-queens puzzle and produces
;; the neighbours of cand as a list of candidates according to the following algorithm:
;;   Generate one new neighbour Candidate of cand for each unoccupied square on the board that is not
;;   attacking a queen position already in cand. Add the new position to the front of cand. Candidates
;;   in the produces list may be in any order. Produce empty if there are no valid neighbours.
;; Example:
(check-expect (neighbours-naive '((0 0)) 4)
              '(((2 3) (0 0)) ((1 3) (0 0)) ((3 2) (0 0)) ((1 2) (0 0))
                              ((3 1) (0 0)) ((2 1) (0 0))))

;; neighbours-naive: Candidate Nat -> (listof Candidate)
(define (neighbours-naive cand n)
  ;; The board completes row by row. (0,0) (1,0) (2,0) ... (0,1) (1,1)...
  (local [(define board (foldr (lambda (x y) (append x y)) empty
                               (build-list n (lambda (x) (build-list n (lambda (y) (list y x)))))))
          (define cands (foldl (lambda (x y) (append x y)) empty cand))
          (define (attacking-any? p1 cand)
            (cond
              [(empty? cand) false]
              [(attacking? p1 (first cand)) true]
              [else (attacking-any? p1 (rest cand))]))
                   
          (define (find-candidates board locand)
            (cond
              [(empty? board) locand]
              [(attacking-any? (first board) cand) (find-candidates (rest board) locand)]
              [else (find-candidates (rest board)
                                     (cons (append (list (first board)) cand)
                                           locand))]))]
    (find-candidates board empty)))

;; Tests
(check-expect (neighbours-naive '((1 0) (2 3)) 5)
              '(((4 4) (1 0) (2 3)) ((0 4) (1 0) (2 3)) ((4 2) (1 0) (2 3))
                                    ((0 2) (1 0) (2 3)) ((3 1) (1 0) (2 3))))
(check-expect (neighbours-naive '((1 0)) 4)
              '(((3 3) (1 0)) ((2 3) (1 0)) ((0 3) (1 0)) ((2 2) (1 0))
                              ((0 2) (1 0)) ((3 1) (1 0))))
;; d)
;; (neighbours-row cand n) consumes the same parameters as c, but produces the candidates according
;; to the following algorithm:
;;   Generate one new neighbour Candidate of cand for each unoccupied square in the
;;   unoccupied row with lowest index that is not attacking a queen position already in
;;   cand. Add the new Position to the front of cand. You may assume that the first
;;   Position in cand is in its highest occupied row. Candidates in the produced list may
;;   be in any order.
;; Example:
(check-expect (neighbours-row '((0 0)) 4)
              '(((1 3) (0 0)) ((1 2) (0 0))))

;; neighbours-row: Candidate Nat -> (listof Candidates)
(define (neighbours-row cand n)
  (local
    [(define board
       (cond [(empty? cand)
              (build-list n (lambda (y) (list 0 y)))]
             [else (build-list n (lambda (y) (list (+ 1 (first (first cand))) y)))])) 
     (define cands (foldl (lambda (x y) (append x y)) empty cand))
     (define (attacking-any? p1 cand)
       (cond
         [(empty? cand) false]
         [(attacking? p1 (first cand)) true]
         [else (attacking-any? p1 (rest cand))]))
    
     (define (find-row-cands board locand)
       (cond
         [(empty? board) locand]
         [(attacking-any? (first board) cand) (find-row-cands (rest board) locand)]
         [else (find-row-cands (rest board)
                               (cons (append (list (first board)) cand)
                                     locand))]))]
    (find-row-cands board empty)))

;; Tests:
(check-expect (neighbours-row '((1 0)) 4)
              '(((2 3) (1 0)) ((2 2) (1 0))))
(check-expect (neighbours-row '((2 3) (1 0)) 5)
              '(((3 1) (2 3) (1 0))))
(check-expect (neighbours-row '((3 1) (2 3) (1 0)) 5)
              '(((4 4) (3 1) (2 3) (1 0))))
(check-expect (neighbours-row '((1 1)) 2) empty)

                              

;; e)
;; (n-queens n nbr-fun) produces a list of all full solutions (candidates) to the N-queens problem
;; of size n, using the functions nbr-fun to find neighbours. Both c) and d) should be able to be
;; consumes as values for nbr-fun.
;;    To solve this problem, you will need to modify the find-path and find-path/list
;;    functions given in Module 18. Think carefully about how this problem differs from
;;    backtracking in an explicit graph. What is our origin? How do we know when we have
;;    reached a destination?
;; Example:
(check-expect (n-queens 4 neighbours-row)
              (list (list (list 3 2) (list 2 0) (list 1 3) (list 0 1))
                    (list (list 3 1) (list 2 3) (list 1 0) (list 0 2))))

;; n-queens: Nat (Candidate Nat -> (listof Candidates)) -> (listof Candidates)
(define (n-queens n nbr-fun)
  (local [
          ;; (find-path orig) determines all the possible combinations of queens, including dupes.
          ;; find-path: Node Node Graph -> (anyof (listof Node) false)
          (define (find-path orig)
            (cond
              [(= (length orig) n) (list orig)]
              [else
               (local
                 [(define nbrs (nbr-fun orig n))
                  (define ?path (find-path/list nbrs))]
                 (foldr append empty (list ?path)))]))

          ;; (find-path/list nbrs) helps to determine the possible combinations of queens.
          ;; find-path/list: (listof Node) Node Graph (listof Node) -> Result
          (define (find-path/list nbrs)
            (cond
              [(empty? nbrs) empty]
              [else
               (local [(define path? (find-path (first nbrs)))]
                 (cond
                   [(empty? path?) (find-path/list (rest nbrs))]
                   [else 
                    (append path? (find-path/list (rest nbrs)))]))]))
          ;; (filter-lsts lst acc) removes dupes from the lst.
          ;; filter-lsts: (listof Candidate) empty -> (listof Candidate)
          (define (filter-lsts lst acc)
            (cond
              [(empty? lst) acc]
              [(same? (first lst) (rest lst)) (filter-lsts (rest lst) acc)]
              [else (filter-lsts (rest lst) (cons (first lst) acc))]))
          ;; (same? cand lst) determines if a candidates has a dupe in the list.
          ;; same?: Candidate (listof Candidate) -> Bool
          (define (same? cand lst)
            (cond
              [(empty? lst) false]
              [(check cand (first lst)) true]
              [else (same? cand (rest lst))]))

          ;; (check cand1 cand2) determines if two cands are dupes.
          ;; check: Candidate Candidate -> Bool
          (define (check cand1 cand2)
            (cond
              [(empty? cand1) true]
              [(member? (first cand1) cand2) (check (rest cand1) cand2)]
              [else false]))]
    
    (local [(define lst (find-path empty))]
      (cond
        [(empty? lst) '(())]
        [else (filter-lsts lst empty)]))))


;; Tests;
(check-expect (n-queens 4 neighbours-naive)
              (list (list (list 2 3) (list 0 2) (list 3 1) (list 1 0))
                    (list (list 1 3) (list 3 2) (list 0 1) (list 2 0))))

(check-expect (n-queens 6 neighbours-naive)
              (list
               (list (list 4 5) (list 2 4) (list 0 3) (list 5 2) (list 3 1) (list 1 0))
               (list (list 3 5) (list 0 4) (list 4 3) (list 1 2) (list 5 1) (list 2 0))
               (list (list 2 5) (list 5 4) (list 1 3) (list 4 2) (list 0 1) (list 3 0))
               (list (list 1 5) (list 3 4) (list 5 3) (list 0 2) (list 2 1) (list 4 0))))

(check-expect (n-queens 6 neighbours-row)
              (list
               (list (list 5 4) (list 4 2) (list 3 0) (list 2 5) (list 1 3) (list 0 1))
               (list (list 5 3) (list 4 0) (list 3 4) (list 2 1) (list 1 5) (list 0 2))
               (list (list 5 2) (list 4 5) (list 3 1) (list 2 4) (list 1 0) (list 0 3))
               (list (list 5 1) (list 4 3) (list 3 5) (list 2 0) (list 1 2) (list 0 4))))

(check-expect (n-queens 3 neighbours-row) (list empty))
(check-expect (n-queens 3 neighbours-naive) (list empty))

(check-expect (n-queens 2 neighbours-naive) (list empty))

(check-expect (n-queens 1 neighbours-naive) '(((0 0))))

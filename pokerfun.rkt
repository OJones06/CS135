;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pokerfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 04, Problem 3 "pokerfun.rkt"
;; ***************************************************

(define-struct card (rank suit))
;; A Card is a (make-card (anyof Int Sym) Sym)
;; Requires: rank is either an integer from 2 to 10
;; or one of 'Jack, 'Queen, 'King, 'Ace
;; suit is one of 'Club, 'Diamond, 'Heart, 'Spade

;; (card-number card) consumes a Card and produces a number representing
;; the cards value (useful for royal cards)
;; Example:

(check-expect (card-number (make-card 'King 'Spade)) 13)

;; card-number: Card -> Num
(define (card-number card)
  (cond
    [(number? (card-rank card)) (card-rank card)]
    [(symbol=? (card-rank card) 'Jack) 11]
    [(symbol=? (card-rank card) 'Queen) 12]
    [(symbol=? (card-rank card) 'King) 13]
    [else 14])) ; Ace

;; (suit-number card) consumes a card and produces a number representing
;; the suit's value (Club, Diamond, Heart, Spade) -> (1, 2, 3, 4)
;; Example:

(check-expect (suit-number (make-card 'King 'Spade)) 4)

;; suit-number: Card -> Num
(define (suit-number card)
  (cond
    [(symbol=? (card-suit card) 'Club) 1]
    [(symbol=? (card-suit card) 'Diamond) 2]
    [(symbol=? (card-suit card) 'Heart) 3]
    [else 4])) ; Spade
                                
;; a)
;; (sorted? cards) consumes a list of cards and produces true if the list of cards is
;; sorted and false if it is not sorted.
;; Example:

(check-expect (sorted?
               (cons (make-card 5 'Heart)
                     (cons (make-card 6 'Spade)
                           (cons (make-card 'Jack 'Diamond)
                                 (cons (make-card 'Jack 'Heart)
                                       (cons (make-card 'Ace 'Club)
                                             (cons (make-card 'Ace 'Diamond) empty)))))))
              true)

;; sorted?: (listof Cards) -> Bool
(define (sorted? cards)
  (cond
    [(empty? cards) true]
    [(empty? (rest cards)) true]
    [(> (card-number (first cards)) (card-number (first (rest cards)))) false]
    [(= (card-number (first cards)) (card-number (first (rest cards))))
     (cond
       [(> (suit-number (first cards)) (suit-number (first (rest cards)))) false]
       [else (sorted? (rest cards))])]
    [else (sorted? (rest cards))]))
;; Tests

(check-expect (sorted?
               (cons (make-card 5 'Heart)
                     (cons (make-card 5 'Spade)
                           (cons (make-card 5 'Diamond) empty))))
              false)
(check-expect (sorted?
               (cons (make-card 3 'Heart)
                     (cons (make-card 4 'Heart)
                           (cons (make-card 5 'Heart) empty))))
              true)
(check-expect (sorted?
               (cons (make-card 'Queen 'Diamond)
                     (cons (make-card 3 'Heart)
                           (cons (make-card 6 'Diamond) empty))))
              false)
(check-expect (sorted? empty) true)

;; b)
;; (find-card cards) consumes a list of cards and produces true if the first card has a
;; duplicate and false otherwise.
;; Example:
(check-expect (find-card
               (cons (make-card 5 'Diamond)
                     (cons (make-card 6 'Heart)
                           (cons (make-card 6 'Heart) empty))))
              true)
;; find-card: (listof Card) -> Bool
(define (find-card cards)
  (cond
    [(empty? (rest cards)) false] ; base case
    [(= (card-number (first cards)) (card-number (first (rest cards)))) ; if numbers are same
     (cond
       [(= (suit-number (first cards)) (suit-number (first (rest cards)))) true] ; same suit
       [(empty? (rest (rest cards))) (find-card (rest cards))]
       [else (find-card (cons (first cards) (rest (rest cards))))])]
    [else (find-card (rest cards))]))

;; (cheater? cards) consumes a list of cards and produces true if
;; the list of cards contains a duplicate and false otherwise.
;; Example:
(check-expect (cheater?
               (cons (make-card 'King 'Heart)
                     (cons (make-card 'King 'Heart)
                           (cons (make-card 'King 'Spade)
                                 (cons (make-card 'Ace 'Diamond) empty)))))
              true)
;; cheater?: (listof Card) -> Bool
(define (cheater? cards)
  (cond
    [(empty? cards) false]
    [(empty? (rest cards)) false]
    [(find-card cards) true]
    [else (cheater? (rest cards))]))
;; Tests
(check-expect (cheater?
               (cons (make-card 3 'Diamond)
                     (cons (make-card 4 'Diamond)
                           (cons (make-card 5 'Diamond)
                                 (cons (make-card 6 'Diamond) empty)))))
              false)
(check-expect (cheater?
               (cons (make-card 7 'Club)
                     (cons (make-card 7 'Diamond)
                           (cons (make-card 7 'Spade)
                                 (cons (make-card 7 'Spade) empty)))))
              true)
(check-expect (cheater? empty) false)

;; c)

;; (is-straight? cards) consumes a sorted list of 5 cards and produces
;; true if the cards are increasing by one rank in order and false otherwise.
;; Example:
(check-expect (is-straight?
               (cons (make-card 7 'Diamond)
                     (cons (make-card 8 'Spade)
                           (cons (make-card 9 'Heart)
                                 (cons (make-card 10 'Spade)
                                       (cons (make-card 'Jack 'Heart) empty))))))
              true)
;; is-straight?: (listof Card) -> Bool
(define (is-straight? cards)
  (cond
    [(empty? (rest cards)) true]
    [(= 1 (- (card-number (first (rest cards))) (card-number (first cards))))
     (is-straight? (rest cards))]
    [else false]))
;; Tests: 
(check-expect (is-straight?
               (cons (make-card 'Ace 'Heart)
                     (cons (make-card 2 'Diamond)
                           (cons (make-card 3 'Heart)
                                 (cons (make-card 4 'Club)
                                       (cons (make-card 5 'Space) empty))))))
              false)
(check-expect (is-straight?
               (cons (make-card 10 'Diamond)
                     (cons (make-card 'Jack 'Diamond)
                           (cons (make-card 'Queen 'Diamond)
                                 (cons (make-card 'King 'Diamond)
                                       (cons (make-card 'Ace 'Diamond) empty))))))
              true)

;; d)

;; (is-flush? cards) consumes a list of 5 Cards and produces true if the 5 cards
;; are the same suit and false otherwise.
;; Example:
(check-expect (is-flush?
               (cons (make-card 10 'Diamond)
                     (cons (make-card 'Jack 'Diamond)
                           (cons (make-card 'Queen 'Diamond)
                                 (cons (make-card 'King 'Diamond)
                                       (cons (make-card 'Ace 'Diamond) empty))))))
              true)
;; is-flush?: (listof Card) -> Bool
(define (is-flush? cards)
  (cond
    [(empty? (rest cards)) true]
    [(symbol=? (card-suit (first cards)) (card-suit (first (rest cards)))) (is-flush? (rest cards))]
    [else false]))
;; Tests:
(check-expect (is-flush?
               (cons (make-card 2 'Spade)
                     (cons (make-card 5 'Spade)
                           (cons (make-card 7 'Spade)
                                 (cons (make-card 'King 'Spade)
                                       (cons (make-card 'Ace 'Spade) empty))))))
              true)
(check-expect (is-flush?
               (cons (make-card 2 'Spade)
                     (cons (make-card 3 'Diamond)
                           (cons (make-card 4 'Club)
                                 (cons (make-card 5 'Spade)
                                       (cons (make-card 6 'Club) empty))))))
              false)

;; e)
;; (is-full-house? cards) consumes a list of 5 cards and produces true if there is
;; a two pair and three pair and false otherwise
;; Example:
(check-expect (is-full-house?
               (cons (make-card 2 'Spade)
                     (cons (make-card 2 'Diamond)
                           (cons (make-card 'Queen 'Club)
                                 (cons (make-card 'Queen 'Diamond)
                                       (cons (make-card 'Queen 'Spade) empty))))))
              true)
;; is-full-house?: (listof Card) -> Bool
(define (is-full-house? cards)
  (cond
    [(empty? cards) true]
    [(not (= (card-number (first cards)) (card-number (first (rest cards))))) false]
    [else (cond
            [(and (not (empty? (rest (rest cards))))
                  (= (card-number (first (rest cards)))
                     (card-number (first (rest (rest cards))))))
             (is-full-house? (rest (rest (rest cards))))]
            [else (is-full-house? (rest (rest cards)))])]))
;; Tests:
(check-expect (is-full-house?
               (cons (make-card 2 'Heart)
                     (cons (make-card 3 'Diamond)
                           (cons (make-card 'Jack 'Club)
                                 (cons (make-card 'Queen 'Diamond)
                                       (cons (make-card 'King 'Heart) empty))))))
              false)
(check-expect (is-full-house?
               (cons (make-card 'King 'Spade)
                     (cons (make-card 'King 'Diamond)
                           (cons (make-card 'King 'Club)
                                 (cons (make-card 'Ace 'Diamond)
                                       (cons (make-card 'Ace 'Spade) empty))))))
              true)

;; f)
;; (replace-card card-find replacement-card cards) consumes two Cards and an unsorted list of Cards
;; and produces a list of cards where whenever the first consumed Card occurs
;; it is replaced my the second consumed Card.
;; Example:
(check-expect (replace-card (make-card 2 'Heart) (make-card 'Ace 'Diamond)
                            (cons (make-card 2 'Heart)
                                  (cons (make-card 7 'Spade)
                                        (cons (make-card 2 'Heart)
                                              (cons (make-card 2 'Heart) empty)))))
              (cons (make-card 'Ace 'Diamond)
                    (cons (make-card 7 'Spade)
                          (cons (make-card 'Ace 'Diamond)
                                (cons (make-card 'Ace 'Diamond) empty)))))
;; replace-card: Card Card (listof Cards) -> (listof Cards)
(define (replace-card card-find replacement-card cards)
  (cond
    [(empty? cards) empty]
    [(and (symbol=? (card-suit (first cards)) (card-suit card-find))
     (= (card-number (first cards)) (card-number card-find)))
     (cons replacement-card (replace-card card-find  replacement-card (rest cards)))]
    [else (cons (first cards) (replace-card card-find replacement-card (rest cards)))]
    ))

(check-expect (replace-card (make-card 'Jack 'Spade) (make-card 7 'Spade)
                            (cons (make-card 2 'Heart)
                                  (cons (make-card 7 'Spade)
                                        (cons (make-card 2 'Heart)
                                              (cons (make-card 'Jack 'Spade) empty)))))
              (cons (make-card 2 'Heart)
                                  (cons (make-card 7 'Spade)
                                        (cons (make-card 2 'Heart)
                                              (cons (make-card 7 'Spade) empty)))))
(check-expect (replace-card (make-card 2 'Heart) (make-card 7 'Spade)
                            (cons (make-card 2 'Heart)
                                  (cons (make-card 7 'Spade)
                                        (cons (make-card 2 'Heart)
                                              (cons (make-card 7 'Spade) empty)))))
              (cons (make-card 7 'Spade)
                    (cons (make-card 7 'Spade)
                          (cons (make-card 7 'Spade)
                                (cons (make-card 7 'Spade) empty)))))
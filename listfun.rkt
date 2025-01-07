;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 04, Problem 2 "listfun.rkt"
;; ***************************************************

;; a)
;; (number-of-sumbols list) consumes a (listof (anyof Num Str Bool Sym))
;; and produces a number representing how many symbols are in the list.
;; Examples:
(check-expect (number-of-symbols
               (cons 'hey
                     (cons 'its
                           (cons 'me
                                 (cons 'ALIA empty)))))
              4)
;; number-of-symbols: list -> Num
(define (number-of-symbols list)
  (cond
    [(empty? list) 0]
    [(symbol? (first list)) (+ 1 (number-of-symbols (rest list)))]
    [else 0]))

;; (list-has-exactly-4-symbols list) consumes a (listof (anyof Num Str Bool Sym))
;; and produces a boolean representing if there are 4 symbols in the list or not.
;; Examples:
(check-expect (list-has-exactly-4-symbols
               (cons 'hey
                     (cons 'its
                           (cons 'me
                                 (cons 'ALIA empty)))))
              true)
;; list-has-exactly-4-symbols: List -> Bool
(define (list-has-exactly-4-symbols list)
  (cond
    [(= (number-of-symbols list) 4) true]
    [else false]))
;; Tests
(check-expect (list-has-exactly-4-symbols
               (cons "this"
                     (cons 'example
                           (cons 'has
                                 (cons 4
                                       (cons 'symbols
                                             (cons "in it"
                                                   (cons False empty))))))))
              false)
(check-expect (list-has-exactly-4-symbols
               (cons 'one
                     (cons 'two
                           (cons 3
                                 (cons 4
                                       (cons 5 empty))))))
              false)

;; b)
;; (add-only-numbers list) consumes a (listof (anyof Num Str Bool Sym)) and produces
;; a number representing all of the terms of the list that are numbers added up.
;; Examples:
(check-expect (add-only-numbers
               (cons 50
                     (cons "51"
                           (cons 12
                                 (cons "5"
                                       (cons 'fifty-two empty))))))
              62)
;; add-only-numbers: (listof (anyof Num Str Bool Sym)) -> Num
(define (add-only-numbers list)
  (cond
    [(empty? list) 0]
    [(number? (first list)) (+ (first list) (add-only-numbers (rest list)))]
    [else (add-only-numbers (rest list))]))
;; Tests
(check-expect (add-only-numbers
               (cons -50
                     (cons 25
                           (cons 72
                                 (cons 0 empty)))))
              47)
(check-expect (add-only-numbers
               (cons 10.5 empty))
              10.5)

;; c)
;; (before-after str-find str-before str-after list) consumes three strings,
;; and a (listof Str) and produces a new list where whenever str-find occurs,
;; str-before will occur before it and str-after will occur after.
;; Examples:
(check-expect
 (before-after "lets" "go" "broncos"
               (cons "I"
                     (cons "say"
                           (cons "lets"
                                 (cons "go" empty)))))
 (cons "I"
       (cons "say"
             (cons "go"
                   (cons "lets"
                         (cons "broncos"
                               (cons "go" empty)))))))
;; before-after: Str Str Str (listof Str) -> (listof Str)
(define (before-after str-find str-before str-after list)
  (cond
    [(empty? list) empty]
    [(string=? (first list) str-find)
     (cons str-before
           (cons str-find
                 (cons str-after
                       (before-after str-find str-before str-after (rest list)))))]
    [else (cons (first list) (before-after str-find str-before str-after (rest list)))]))
;; Tests:
(check-expect
 (before-after "booooo" "chiefs" "they"
               (cons "The"
                     (cons "booooo"
                           (cons "suck"
                                 (cons "yuck" empty)))))
 (cons "The"
       (cons "chiefs"
             (cons "booooo"
                   (cons "they"
                         (cons "suck"
                               (cons "yuck" empty)))))))
(check-expect
 (before-after "United" "States of" "America"
               (cons "God"
                     (cons "bless"
                           (cons "the"
                                 (cons "USA" empty)))))
 (cons "God"
       (cons "bless"
             (cons "the"
                   (cons "USA" empty)))))

;; d)
;; (same-type? target list-item) consumes (anyof Num Str Sym) and (anyof Num Str Sym)
;; and produces true if the consumed values have the same type and false otherwise.
;; Examples:
(check-expect (same-type? "same" "type") true)
;; same-type?: (anyof Num Str Sym) (anyof Num Str Sym) -> Bool
(define (same-type? target list-item)
  (cond
    [(symbol? target) (symbol? list-item)]
    [(number? target) (number? list-item)]
    [(string? target) (string? list-item)]))

;; (exists? target list) consumes (anyof Num Str Sym) and (listof(anyof Num Str Sym))
;; and produces true if the consumed value is in the list, and false otherwise
;; Examples:
(check-expect (exists? 'I
                       (cons 'you
                             (cons 'me
                                   (cons "myself"
                                         (cons 9
                                               (cons 'I
                                                     (cons false empty)))))))
              true)
;; exists?: (anyof Num Str Sym) (listof (anyof Num Str Sym)) -> Bool
(define (exists? target list)
  (cond
    [(empty? list) false]
    [(not (same-type? target (first list))) (exists? target (rest list))]
    [else (cond
            [(and (number? target) (= target (first list))) true]
            [(and (string? target) (string=? target (first list))) true]
            [(and (symbol? target) (symbol=? target (first list))) true]
            [else (exists? target (rest list))])]))

(check-expect (exists? "bruv"
                       (cons "bruh"
                             (cons "boi"
                                   (cons "bro"
                                         (cons "bud" empty)))))
              false)
(check-expect (exists? 'sixty-two
                       (cons 62
                             (cons "62"
                                   (cons "sixty-two"
                                         (cons (+ 60 2)
                                               (cons 'sixty-three empty))))))
              false)

;; e)
;; (remove-duplicates list) consumes a (listof(anyof Num Str Sym)) and produces a
;; (listof(anyof Num Str Sym)) with the duplicates from the original list removed.
;; Examples:
(check-expect (remove-duplicates
               (cons 5
                     (cons 6
                           (cons 7
                                 (cons 'seven
                                       (cons 6
                                             (cons 5
                                                   (cons 4 empty))))))))
              (cons 7
                    (cons 'seven
                          (cons 6
                                (cons 5
                                      (cons 4 empty))))))
;; remove-duplicates: (listof(anyof Num Str Sym)) -> (listof(anyof Num Str Sym))
(define (remove-duplicates list)
  (cond
    [(empty? list) empty]
    [(exists? (first list) (rest list)) (remove-duplicates (rest list))]
    [else (cons (first list) (remove-duplicates (rest list)))]))
;; Tests: 
(check-expect (remove-duplicates
               (cons "Go"
                     (cons "Broncos!"
                           (cons "Go"
                                 (cons "Broncos!"
                                       (cons "Whoop"
                                             (cons 'Whoop
                                                   (cons '! empty))))))))
              (cons "Go"
                    (cons "Broncos!"
                          (cons "Whoop"
                                (cons 'Whoop
                                      (cons '! empty))))))
(check-expect (remove-duplicates
               (cons 'yo
                     (cons 'yo
                           (cons 'yo
                                 (cons "Joe"
                                       (cons 'Joe
                                             (cons 'joe empty)))))))
              (cons 'yo
                    (cons "Joe"
                          (cons 'Joe
                                (cons 'joe empty)))))
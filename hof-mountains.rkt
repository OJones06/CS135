;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hof-mountains) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 09 Problem 2: "hof-mountains.rkt"
;; ***************************************************

;; Q4.

;; a)

;; (min-max comp lst) consumes a comparator and a single non-empty list and yiels a fixed-length
;; list containing the minimum and maximum values of the list. 
;; Requires:
;; lst is a non-empty list.
;; The comparator must accept two parameters a and b of the same type and yield true if a is smaller
;; than b and false otherwise. If they are equal, pick the first one.
;; Example:
(check-expect (min-max < (list 1 2 3 7 8 9 4 5 6)) (list 1 9))

;; min-max: (X X -> Bool) (listof X) -> (list X X)
(define min-max
  (lambda (comp lst)
    (list (foldr (lambda (x y) (cond [(comp y x) y]
                                                  [else x])) (first lst) lst)
          (foldr (lambda (x y) (cond [(comp x y) y]
                                                  [else x])) (first lst) lst))))

;; Tests:
(check-expect (min-max < (list 1 5 7 6 0 3 7 4 2)) (list 0 7))
(check-expect (min-max < (list 1 5 7 6 0 3 4 2)) (list 0 7))
(check-expect (min-max
               (lambda (str-a str-b) (< (string-length str-a)
                                        (string-length str-b)))
               (list "CS135" "Is" "Lots" "Of" "Fun"))
              (list "Is" "CS135"))
(check-expect (min-max
               (lambda (str-a str-b) (< (string-length str-a)
                                        (string-length str-b)))
               (list "one" "to" "th" "five" "sixs" "one"))
              (list "to" "five"))

;; b)

;; (in-order? comp lst) consumes a comparator and a list and yields true if the elements are in order
;; and false otherwise.
;; Example:
(check-expect (in-order? string<? (list "Carrie" "Ethan" "Nick" "Owen" "Todd")) true)

;; in-order?: (X X -> Bool) (listof X) -> Bool
(define (in-order? comp lst)
  (cond
    [(empty? lst) true]
    [(empty? (rest lst)) true]
    [else (second
           (foldl (lambda (x y)
                    (list x (and (second y) (comp (first y) x))))
                  (list (first lst) true)
                  (rest lst)))]))



;; create filter that removes any out of order items.
     
;; Tests:
(check-expect (in-order? < (list 0 1 2 3 5 5 135)) false)
(check-expect (in-order? < (list 0 1 2 3 5 135)) true)
(check-expect (in-order? < (list 0 1 2 3 5 135 -1)) false)
(check-expect (in-order?
               (lambda (str-a str-b) (< (string-length str-a)
                                        (string-length str-b)))
               (list "A" "short" "wordlist" "for y'all"))
              true)
(check-expect (in-order? > '(1)) true)
(check-expect (in-order? string<? empty) true)


;; c)

;; (slice a b lst) consumes two numbers and a list, and produces sub-list of the original list from
;; the a-th to b-th element of the original list (inclusive).
;; Example:
(check-expect (slice 2 5 '(a b c d e f g h i j)) '(c d e f))

;; slice: Nat Nat (listof X) -> (listof X)
(define (slice a b lon)
  (filter (lambda (x) (not (cons? (filter (lambda (y) (equal? x y))
                                          ((lambda (lst1 lst2)
                                             (foldr cons lst2 lst1))
                                           (foldl (lambda (x y) (cons x y)) empty
                                                  (foldl (lambda (x result)
                                                           (cond
                                                             [(> (length result) (- a 1)) result]
                                                             [else (cons x result)]))
                                                         empty
                                                         lon))
                                           (foldr (lambda (x result)
                                                    (cond
                                                      [(> (length result) (- (length lon) b 2))
                                                       result]
                                                      [else (cons x result)]))
                                                  empty
                                                  lon))))))
          lon))

;; basic idea:
; need to build a list of 0-b
; build a list from a-b

;; Tests:
(check-expect (slice 1 4 (list 1 2 3 4 5 6 7)) (list 2 3 4 5))
(check-expect (slice 0 0 (list "CS135" "Is" "Fun")) (list "CS135"))


;; d)

;; (split-n n lst) consumes a Nat and a list and produces a list of n sub-lists of the original list
;; Pay attention to how the values of the original list are distributed.
;; Example:
(check-expect (split-n 4 '(1 2 3 4 5 6 7 8 9 10)) '((1 5 9) (2 6 10) (3 7) (4 8)))
;; split-n: Nat (listof X) -> (listof (listof X))
(define (split-n n lst)
  (map
    (lambda (k)
      (foldr
        (lambda (x y z)
          (cond
            [(= (modulo y n) k) (cons x z)]
            [else z]))
        empty
        lst
        (build-list (length lst) (lambda (x) x))))
    (build-list n (lambda (x) x))))


;; Tests:
(check-expect (split-n 3 (list 1 2 3 4 5 6 7 8 9 10 11 12))
              (list (list 1 4 7 10) (list 2 5 8 11) (list 3 6 9 12)))
(check-expect (split-n 5 (list 1 2 3 4 5 6 7 8 9 10 11 12))
              (list (list 1 6 11) (list 2 7 12) (list 3 8) (list 4 9)
                    (list 5 10)))
(define my-lst (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(check-expect (split-n 3 my-lst)
              (list (list 1 4 7 10 13)
                    (list 2 5 8 11 14)
                    (list 3 6 9 12 15)))
(check-expect (split-n 4 my-lst)
              (list (list 1 5 9 13) (list 2 6 10 14)
                    (list 3 7 11 15) (list 4 8 12)))
(check-expect (split-n 5 my-lst)
              (list (list 1 6 11) (list 2 7 12) (list 3 8 13)
                    (list 4 9 14) (list 5 10 15)))
(check-expect (split-n 6 my-lst)
              (list (list 1 7 13) (list 2 8 14) (list 3 9 15)
                    (list 4 10) (list 5 11) (list 6 12)))
(check-expect (split-n 2 empty) '(() ()))
(check-expect (split-n 4 '(1 2 3)) '((1) (2) (3) ()))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hof-hills) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 09 Problem 2: "hof-hills.rkt"
;; ***************************************************

;; Q3.

;; a)

;; (pocket-change losym) consumes a (listof Sym) and produces a number that is the total value of the
;; change.
;; Example:
(check-expect (pocket-change (list 'dime 'quarter 'loonie 'loonie 'twonie)) 4.35)

;; pocket-change: (listof Sym) -> Num
(define (pocket-change losym)
  (foldr
   (lambda (x y) (cond
                 [(symbol=? 'nickel x) (+ y .05)]
                 [(symbol=? 'dime x) (+ y .1)]
                 [(symbol=? 'quarter x) (+ y .25)]
                 [(symbol=? 'loonie x) (+ y 1)]
                 [(symbol=? 'twonie x) (+ y 2)]
                 [else (+ y 0)]))
   0
   losym))
;; Test:
(check-expect (pocket-change (list 'loonie 'watcard 'button 'nickel
                                   'dime 'gum)) 1.15)


;; b)

;; (make-validator losym) consumes a (list Sym) and creates a new function that checks if a symbol
;; exists on a list. The newly created function has one symbol-type parameter and yields a boolean.
;; Example:
(check-expect ((make-validator '(this that the other one)) 'other) true)

;; make-validator: (listof Sym) -> (Sym -> Bool)
(define (make-validator losym)
  (lambda (symbol) (not (empty? (filter (lambda (x) (symbol=? x symbol)) losym)))))
;; Tests:
(check-expect ((make-validator (list '+ '- '*)) '*) true)
(check-expect ((make-validator (list 'This 'is 'Sparta)) 'Athens)
false)


;; c)

;; (remove-outliars lon) consumes a non empty (listof Num) and produces the same list with any
;; outliars removes. An outliar is a number that is more that 2 standard deviations from the mean.
;; Example:
(check-expect (remove-outliers '(128 255 1222 54 22 67 780 623 584 257))
              '(128 255 54 67 623 584 257))
;; remove-outliars: (listof Num) -> (listof Num)
(define (remove-outliers lon)
  (local
    [(define mean (/ (foldr + 0 lon) (length lon)))
     (define standard-deviation (sqrt (/ (foldr (lambda (x y) (+ y (sqr (- x mean))))
                                                0 lon) (length lon))))]
    (filter ((lambda (a b) (lambda (x) (and (< a x) (< x b))))
             (- mean standard-deviation)
             (+ mean standard-deviation)) lon)))

;; Tests:
(check-expect (remove-outliers (list 1 2 3 1 2 1 3 10 2 -5))
              (list 1 2 3 1 2 1 3 2))


;; d)
;; (primes n) consumes a Nat and generates a list of primes from 2 to n.
;; Requires:
;; n >= 2
;; Example:
(check-expect (primes 32) '(2 3 5 7 11 13 17 19 23 29 31))

;; primes: Num -> (listof Num)
(define (primes n)
  (filter (lambda (n)
            (empty? (filter (lambda (x) (= 0 (remainder n x)))
                                      (build-list (- n 2) (lambda (i) (+ i 2))))))
          (rest (build-list n (lambda (x) (+ x 1))))))

;; general idea:
; build a list of 2 -> n
; remove non-primes.
; make prime? predicate

;; Tests:
(check-expect (primes 10) (list 2 3 5 7))
(check-expect (primes 30) (list 2 3 5 7 11 13 17 19 23 29))
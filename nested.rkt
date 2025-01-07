;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nested) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 08 Problem 5: "nested.rkt"
;; ***************************************************

;; Q5:

;; A (nested-listof X) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))
;; Requires: X itself is not a list type

;; a)
;; Write a template function named nested-listof-X-template that processes a (nested-listof X).
;; nested-listof-X-template: (nested-listof-X) -> Any
(define (nested-listof-X-template nestlst)
  (cond
    [(empty? nestlst) ...]
    [(cons? (first nestlst)) (... (nested-listof-X-template (first nestlst))
                             (nested-listof-X-template (rest nestlst)))]
    [(... (first nestlst)) (... (first nestlst)
                           (nested-listof-X-template (rest nestlst)))]
    [else (...(first nestlst)
              (nested-listof-X-template (rest nestlst)))]))
;; b)
;; (nested-filter pred nestlst) consumes a predicate function and a nested list and removes every
;; element that appears anywhere in the nested list where the predicate is false.
;; Example:
(check-expect (nested-filter symbol? (list 'this 'that "the other" (list 9 8 'seven)))
              (list 'this 'that (list 'seven)))
;; nested-filter: (X -> Bool) (neted-listof X) -> (nested-listof X)
(define (nested-filter pred nestlst)
  (cond
    [(empty? nestlst) empty]
    [(cons? (first nestlst)) (cons (nested-filter pred (first nestlst))
                                   (nested-filter pred (rest nestlst)))]
    [(pred (first nestlst)) (cons (first nestlst) (nested-filter pred (rest nestlst)))]
    [else (nested-filter pred (rest nestlst))]))
;; Tests:
(check-expect (nested-filter number? (list 5 6 7 8 '(list nine ten 11)))
              (list 5 6 7 8 (list 11)))

;; c)
;; (ruthless nestlosym) consumes a (nested-listof Sym) and produces an identical list with all
;; instances of 'ruth removed.
;; Example:
(check-expect (ruthless '(blue green red ruth blue (gray) (ruth)))
              '(blue green red blue (gray) ()))
;; ruthless: (nested-listof Sym) -> (nested-listof Sym)
(define (ruthless neslosym)
  (local [(define (not-ruth? sym)
            (not (symbol=? 'ruth sym)))]
    (nested-filter not-ruth? neslosym)))

;; Tests:
(check-expect (ruthless '(rue ruth ra ruth sa suth mu))
              '(rue ra sa suth mu))
;; Given:
(check-expect (ruthless '(rabbit (apple pluto (ruth blue) ruth) hello))
              '(rabbit (apple pluto (blue)) hello))

;; d)
;; (keep-between a b nestlon) consumes two numbers, a and b, and a (nested-listof Num) and produces
;; a (nested-listof Num) keeping the values between a and b inclusive.
;; You must use nested-filter
;; Example:
(check-expect (keep-between -3 5 (list 2 3 4 10 (list -2 -3 -4 -10)))
              (list 2 3 4 (list -2 -3)))
;; keep-between: Num Num (nested-listof Num) -> (nested-listof Num)
(define (keep-between a b nestlon)
  ;; (between_ab? num) consumes a number and produces true if it is between a and b and false if not.
  ;; between_ab?: Num -> Bool
  (local [(define (between_ab? num)
            (and (<= a num) (<= num b)))]
    (nested-filter between_ab? nestlon)))

;; Tests:
(check-expect (keep-between -10 0 (list -17 -13 -11 -7 -5 -3 -2 -1 0 1 3 5))
              (list -7 -5 -3 -2 -1 0))

;; e)
;; (nested-cleanup nestlon) consumes the result from keep-between and removes any empty lists.
;; Example:
(check-expect (nested-cleanup (keep-between -3 5 (list 2 3 4 10 (list -2 -3 -4 -10) (list -11))))
              (list 2 3 4 (list -2 -3)))
;; nested-cleanup: (nested-listof Num) -> (nested-listof Num)
(define (nested-cleanup nestlon)
  (cond
    [(empty? nestlon) false]
    [(list? (first nestlon))
     (cond
       [(equal? (nested-cleanup (first nestlon)) false)
        (nested-cleanup (rest nestlon))]
       [else
        (cons (nested-cleanup (first nestlon))
              (cond
                [(equal? (nested-cleanup (rest nestlon)) false) '()]
                [else (nested-cleanup (rest nestlon))]))])]
    [else
     (cons (first nestlon)
           (cond
             [(equal? (nested-cleanup (rest nestlon)) false) '()]
             [else (nested-cleanup (rest nestlon))]))]))

;; Tests:
(check-expect (nested-cleanup '(1 () 2 () () 3)) '(1 2 3))
(check-expect (nested-cleanup '(1 (()()) 2 ((3 () (()))) ))
              '(1 2 ((3))))
(check-expect (nested-cleanup '()) false)
(check-expect (nested-cleanup '((() (1 2) ()) (3 () 4) ()))
              '(((1 2)) (3 4)))

;; f)
;; (nested-apply lofun nestlon) consumes a (listof Functions) that all have the contract Num -> Num
;; Num -> Int or Num -> Nat, and a (nested-listof Num). It produces a (listof (nested-listof Num))
;; and a (nested-listof Num). The first (nested-listof Num) in the produced list is the result of
;; applying the first function to each number in the consumed (nested-listof Num), the second is the
;; result of applying the second function, and so on...
;; Example:
(check-expect (nested-apply (list abs sqr) '(1 -1 -2 2 0 0 4 (1 5 -5) ()))
              (list '(1 1 2 2 0 0 4 (1 5 5) ())
                    '(1 1 4 4 0 0 16 (1 25 25) ())))
;; nested-apply: (listof Function) (nested-listof Num) -> (nested-listof Num)
(define (nested-apply lofun nestlon)
  ;; (apply-function fun nestlon) consumes a function and a (nested-listof Num) and produces the
  ;; same nested list with the function applied.
  (local
    [(define (apply-function fun nestlon)
       (cond
         [(empty? nestlon) empty]
         [(cons? (first nestlon)) (cons (apply-function fun (first nestlon))
                                        (apply-function fun (rest nestlon)))]
         [(number? (first nestlon)) (cons (fun (first nestlon)) (apply-function fun (rest nestlon)))]
         [else (cons empty (apply-function fun (rest nestlon))) ]))]
    (cond
      [(empty? lofun) empty]
      [else (cons (apply-function (first lofun) nestlon) (nested-apply (rest lofun) nestlon))])))
;; Tests:
(check-expect (nested-apply (list abs floor) '(1.2 (-2 (3.5)) ()))
              (list '(1.2 (2 (3.5)) ()) '(1 (-2 (3)) ())))






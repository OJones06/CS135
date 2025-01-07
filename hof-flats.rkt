;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hof-flats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 09 Problem 2: "hof-flats.rkt"
;; ***************************************************

;; Q2. hof-flats.rkt

;; a)

;; (absolutely-odd loint) consumes a list of integers and produces the sum of the absolute values of
;; the odd integers in the list.
;; Example:
(check-expect (absolutely-odd (list 1 3 5 2 4 7)) 16)

;; absolutely-odd: (listof Int) -> Int
(define (absolutely-odd loint)
  (foldr + 0 (map abs (filter odd? loint))))

;; Tests:
(check-expect (absolutely-odd (list 1 -5 4 6 5)) 11)
(check-expect (absolutely-odd empty) 0)


;; b)

;; (count-n num lon) consumes a number and a listof numbers and produces the number of times that the
;; given number occurs in the list
;; Example:
(check-expect (count-n 5 (list 1 5 2 5 3 4 5)) 3)

;; count-n: Num (listof Num) -> Nat
(define (count-n num lon)
  (length (filter (lambda (x) (= x num)) lon)))

;; Tests:
(check-expect (count-n 1 (list 1 0 1 1 -1 0 1 0 0 1)) 5)
(check-expect (count-n 1 empty) 0)


;; c)

;; (unzip lst) consumes a (listof (listof Num Num)). The first list contains the first element
;; from each pair and the second list contains the second element from each pair in the original
;; order.
;; Example:
(check-expect (unzip (list (list 1 4) (list 2 5) (list 6 7) (list 1 -5)))
              (list (list 1 2 6 1) (list 4 5 7 -5)))

;; unzip: (listof (listof Num Num)) ->(List (listof Num) (listof Num))
(define (unzip lst)
  (list (foldr (lambda (x y) (cons (first x) y)) empty lst)
        (foldr (lambda (x y) (cons (second x) y)) empty lst)))

;; Tests:
(check-expect (unzip (list (list 1 'a) (list 2 'b) (list 3 'c)))
              (list (list 1 2 3) (list 'a 'b 'c)))
(check-expect (unzip empty) (list empty empty))


;; d)

;; (dedup lon) consumes a list of numbers and produces a new list with only the first occurence
;; of each element of the original list.
;; Example:
(check-expect (dedup (list 1 2 2 3 1 5 6 6 5)) (list 1 2 3 5 6))

;; dedup: (listof Num) -> (listof Num)
(define dedup
  (lambda (lst)
    ((lambda (lst)
       (foldl
        (lambda (current acc) (cons current acc)) empty lst)) ;; reversing the list

     ;; making the list
     (foldl
      (lambda (current acc)
        (cond [(foldr (lambda (x found) (or found (equal? x current))) false acc) acc]
              [else (cons current acc)]))
      empty
      lst))))



;; Tests:
(check-expect (dedup (list 1 2 1 3 3 2 4)) (list 1 2 3 4))
(check-expect (dedup empty) empty)


;; e)

;; (zero-fill str) consumes a string no longer than 20 characters and produces the same string but
;; with zeros added to the beginning as necessary, so that the string is exactly 20 characters long
;; USE list->string and string->list
;; Requires:
;; length of str <= 20 and >= 0
;; Example:
(check-expect (zero-fill "MYNAMEISOWEN") "00000000MYNAMEISOWEN")

;; zero-fill: Str -> Str
(define (zero-fill str)
  (list->string
   ((lambda (lst1 lst2)
      (foldr cons lst2 lst1))
    (build-list (- 20 (length (string->list str))) (lambda (x) #\0))
    (string->list str))))

;; Tests:
(check-expect (zero-fill "abcdefghijklmn") "000000abcdefghijklmn")
(check-expect (zero-fill "he00llo") "0000000000000he00llo")
(check-expect (zero-fill "") "00000000000000000000")
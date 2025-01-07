;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname median) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 02, Problem 2: "Median of 3"
;; ***************************************************

;; median-of-3-simple takes an input of three numbers and sends the median as the output.

(check-expect (median-of-3-simple 3 4 5) 4)
(check-expect (median-of-3-simple 10 15 2) 10)
(check-expect (median-of-3-simple -2 6 2) 2)
(check-expect (median-of-3-simple 101 -1/7 3) 3)
(check-expect (median-of-3-simple -20 -5 1) -5)
(check-expect (median-of-3-simple 30 20 50) 30)
(check-expect (median-of-3-simple 30 20 0) 20)

;; median-of-3-simple: Num Num Num -> Num
(define (median-of-3-simple a b c)
  (cond
    ;; checks for the three possibilites to which a <= b
    [(<= a b) (cond
                [(<= a c) (cond
                            [(<= b c) b] ;a <= b <= c -> b
                            [else c])] ;a <= c <= b -> c
                [else a])] ;c <= a <= b -> a
    ;; checks for the three possibilities to which a >= b
    [else (cond
            [(<= b c) (cond
                        [(<= a c) a] ;b <= a <= c -> a
                        [else c])] ;b <= c <= a -> c
            [else b])])) ;c <= b <= a -> b
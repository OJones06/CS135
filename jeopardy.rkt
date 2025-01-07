;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Jeopardy!) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024)
;;   Assignment 01, Problem 5: "Jeopardy"
;; ***************************************************

; part a)
(define (min-wager c1 c2 c3)
  ;; Find the maximum value that c2 and c3 could have,
  ;; Then find the amount that c1 would need to bid to beat the max.
  (max (* (- c1 (* c2 2) 1) -1) (* (- c1 (* c3 2) 1) -1)))
;; (min-wager 100000 4000 7000) Testing if the program works correctly

;; part b)
(define (missed-question c1 c2 c3)
  ;; Subtract the result of the min-wager function from the value of c1.
  (- c1 (min-wager c1 c2 c3)))
;; (missed-question 100000 7000 8000) Testing if the program works correctly
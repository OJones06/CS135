;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bin-sqrt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 10 Problem 1: "bin-sqrt.rkt"
;; ***************************************************

;; Q1: bin-sqrt.rkt

;; (bin-sqrt n tol) consumes numbers, n and tol, and produces a number that approximates (sqrt n)
;; within the tolerance of tol. Using the binary search methon described in the question.
;; NO SQRT FUNCTION
;; Example:
(check-within (bin-sqrt 2 0.001) (sqrt 2) 0.001)

;; bin-sqrt: Num Num -> Num
(define (bin-sqrt n tol)
  (local
    [(define k (/ n 2))
     (define (sqrt lower upper)
       (cond
         ;; find what v should b
         [(< (abs (- n (sqr (/ (+ lower upper) 2)))) tol) (/ (+ lower upper) 2)]
         ;; replaces the upper bound
         [(> (sqr (/ (+ lower upper) 2)) n) (sqrt lower (/ (+ lower upper) 2))]
         ;; replaces the lower bound
         [(<= (sqr (/ (+ lower upper) 2)) n) (sqrt (/ (+ lower upper) 2) upper)]))]
    (sqrt 0 n)))

;; Tests:
(check-within (bin-sqrt 128 0.001) (sqrt 128) 0.001)
(check-within (bin-sqrt 567 0.001) (sqrt 567) 0.001)
(check-within (bin-sqrt 1025237261 0.001) (sqrt 1025237261) 0.001)
(check-within (bin-sqrt 0 0.001) (sqrt 0) 0.001)
(check-within (bin-sqrt 162773929188263629128263621 0.001) (sqrt 162773929188263629128263621) 0.001)
(check-within (bin-sqrt 1234 0.001) (sqrt 1234) 0.001)
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname primes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 10 Problem 2: "primes.rkt"
;; ***************************************************

;; Q2: primes.rkt
;; (primes n) consumes a Num and produces a list of all primes from 2-n
;; Example:
(check-expect (primes 20) '(2 3 5 7 11 13 17 19))
;; Requires n > 1

;; primes: Nat -> (listof Num)
(define (primes n)
  (local
    [(define candidates (build-list (- n 1) (lambda (x) (+ x 2))))
     (define (generator candidates lop)
       (cond
         [(empty? candidates) lop]
         [else (generator
                (filter (lambda (x) (not (= (modulo x (first candidates)) 0))) candidates)
                (append lop (list (first candidates))))]))]
    (generator candidates empty)))

;; Tests
(check-expect (primes 2) '(2))
(check-expect (primes 5) '(2 3 5))
(check-expect (primes 21) '(2 3 5 7 11 13 17 19))
(check-expect (primes 22) '(2 3 5 7 11 13 17 19))
(check-expect (primes 15) '(2 3 5 7 11 13))
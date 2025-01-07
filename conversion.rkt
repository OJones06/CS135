;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname speed) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024)
;;   Assignment 01, Problem 4: "Speed"
;; ***************************************************

(define (m/s->mph m/s)
  ;; (m/s) * (1mi / 1609.344 m) * (3600s / 1h) = mi / h
  (* (/ 1 1609.344) 3600 m/s))
;; (m/s->mph 10) Testing if the program works correctly

(define (mph->s/mfn mi/h)
  ;; (mi/h) * (1fn / 3600s) * (1209.6s / 1mfn) (1609.344m / 1mi) * (1smoot / 1.7018m)
  (* mi/h 1/3600 1209.6 1609.344 (/ 1 1.7018)))

;; (mph->s/mfn 1) Testing if the program works correctly
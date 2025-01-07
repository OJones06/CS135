;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname debug-a01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024)
;;   Assignment 01, Problem 1: "Debugging"
;; ***************************************************

(define (luminosity red-pixel green-pixel blue-pixel)
;; FIRST: multiply pixel values by appropriate coefficients, then add the three pixel values together.
  (+ (* 0.3 red-pixel)
     (* 0.59 green-pixel)
     (* 0.11 blue-pixel)))
;; "(luminosity 10 20 30)" testing that the function is written properly.
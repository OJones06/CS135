;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 02, Problem 3: "cond.rkt"
;; ***************************************************

;; Part a)
;; q3a-simplified has the same output of q3a but uses only one 'cond'
;; q3a-simplified: Num Bool -> Num
(define (q3a-simplified n a?)
  (cond
    [(and a? (>= n 0)) (+ n 1)] ; a? is true and n >= 0 -> n + 1
    [a? (- n 1)] ; a? is true and n < 0 -> n - 1
    [else 0])) ; a? is false -> 0

;; Check that the function works as desired.
(check-expect (q3a-simplified 5 true) 6)
(check-expect (q3a-simplified -5 true) -6)
(check-expect (q3a-simplified 5 false) 0)
(check-expect (q3a-simplified 0 true) 1)


;; Part b)
;; q3b-simplified has the same output of q3b but uses only one 'cond'
;; q3b-simplified: Bool Bool Bool -> Symbol
(define (q3b-simplified a? b? c?)
  (cond
    [(or (and a? b? c?) (and a? b?)) 'elm] ; TTT or TTF -> 'elm
    [(and a? c?) 'cedar] ; TFT -> 'cedar
    [a? 'birch] ; TFF -> 'bich
    [(or (and b? c?) b?) 'pine] ; FTT or FTF -> 'pine
    [c? 'cherry] ; FFT -> 'cherry
    [else 'birch])) ; FFF -> 'birch
; Check that the function works as desired.
(check-expect (q3b-simplified true true true) 'elm)
(check-expect (q3b-simplified true true false) 'elm)
(check-expect (q3b-simplified true false true) 'cedar)
(check-expect (q3b-simplified true false false) 'birch)
(check-expect (q3b-simplified false true true) 'pine)
(check-expect (q3b-simplified false true false) 'pine)
(check-expect (q3b-simplified false false true) 'cherry)
(check-expect (q3b-simplified false false false) 'birch)

;; part c)
;; q3c-simplified has the same output of q3c but uses only one 'cond'
;; q3c-simplified: Bool Bool Bool -> Symbol
(define (q3c-simplified a? b? c?)
  (cond
    [(or (and c? b?) (and (not a?) b? (not c?))) 'spruce] ; TTT, FTT, FTF -> 'spruce
    [a? 'hazel] ; TFT, TFF, TTF -> 'hazel
    [c? 'hickory] ; FFT -> 'hickory
    [else 'larch]) ; FFF -> 'larch
  )
; Check that the function works as desired.
(check-expect (q3c-simplified true true true) 'spruce)
(check-expect (q3c-simplified true false true) 'hazel)
(check-expect (q3c-simplified true true false) 'hazel)
(check-expect (q3c-simplified true false false) 'hazel)
(check-expect (q3c-simplified false true true) 'spruce)
(check-expect (q3c-simplified false false true) 'hickory)
(check-expect (q3c-simplified false true false) 'spruce)
(check-expect (q3c-simplified false false false) 'larch)
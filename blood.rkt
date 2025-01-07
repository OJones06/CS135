;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 02, Problem 4: "blood.rkt"
;; ***************************************************

;; part a)
#| can-donate-to/cond? takes the blood type of a donor and potential
   recipient and outputs the compatability (Boolean) without the use of 'and' 'or' 'not'.
   can-donate-to/cond?: Symbol Symbol -> Boolean |#
(define (can-donate-to/cond? donor recipient)
  (cond
    [(symbol=? donor recipient) true] ; When a donor and recipient have the same blood type -> true
    [(symbol=? donor 'O-) true] ; 'O- is the donor -> true
    [(symbol=? recipient 'AB+) true] ; 'AB+ is the recipient -> true
    [(symbol=? donor 'A-) (cond
                               [(symbol=? recipient 'A+) true]
                               [else false])] ; 'A- donor and 'A+ recipient -> true
    ;; ↓↓↓ 'O+ donor and either 'A+ or 'B+ recipient -> true ↓↓↓
    [(symbol=? donor 'O+) (cond
                            [(symbol=? recipient 'A+) true]
                            [(symbol=? recipient 'B+) true]
                            [else false])]
    ;; ↓↓↓ 'B- donor and either 'B+ or 'AB- recipient -> true ↓↓↓
    [(symbol=? donor 'B-) (cond
                            [(symbol=? recipient 'B+) true]
                            [(symbol=? recipient 'AB-) true]
                            [else false])]
    [else false])) ; All other combinations are false

;; part b)
#| can-donate-to/bool? takes the blood type of a donor and potential
   recipient and outputs the compatability (Boolean) without the use of 'cond'.
   can-donate-to/bool?: Symbol Symbol -> Boolean |#
(define (can-donate-to/bool? donor recipient)
  ;; ↓↓↓ If any of the below statements are true -> true, otherwise -> false ↓↓↓
  (or (symbol=? donor recipient) ; When a donor and recipient have the same blood type -> true
      (symbol=? donor 'O-) ; 'O- is the donor -> true
      (symbol=? recipient 'AB+) ; 'AB+ is the recipient -> true
      (and (symbol=? donor 'A-) (symbol=? recipient 'A+)) ; 'A- donor and 'A+ recipient -> true
      ;; ↓↓↓ 'O+ donor and either 'A+ or 'B+ recipient -> true ↓↓↓
      (and (symbol=? donor 'O+) (or (symbol=? recipient 'A+) (symbol=? recipient 'B+)))
      ;; ↓↓↓ 'B- donor and either 'B+ or 'AB- recipient -> true ↓↓↓
      (and (symbol=? donor 'B-) (or (symbol=? recipient 'B+) (symbol=? recipient 'AB-)))))

; Check that the function can-donate-to/bool? works as desired.
(check-expect (can-donate-to/bool? 'O- 'O-) true)
(check-expect (can-donate-to/bool? 'O- 'AB-) true)
(check-expect (can-donate-to/bool? 'O+ 'A-) false)
(check-expect (can-donate-to/bool? 'O+ 'B+) true)
(check-expect (can-donate-to/bool? 'A- 'B-) false)
(check-expect (can-donate-to/bool? 'A- 'A-) true)
(check-expect (can-donate-to/bool? 'A+ 'A-) false)
(check-expect (can-donate-to/bool? 'A+ 'AB+) true)
(check-expect (can-donate-to/bool? 'B- 'O+) false)
(check-expect (can-donate-to/bool? 'B- 'B+) true)
(check-expect (can-donate-to/bool? 'B+ 'AB-) false)
(check-expect (can-donate-to/bool? 'B+ 'AB+) true)
(check-expect (can-donate-to/bool? 'AB- 'A+) false)
(check-expect (can-donate-to/bool? 'AB- 'AB-) true)
(check-expect (can-donate-to/bool? 'AB+ 'B-) false)
(check-expect (can-donate-to/bool? 'A- 'A+) true)
(check-expect (can-donate-to/bool? 'O+ 'A+) true)
(check-expect (can-donate-to/bool? 'B- 'AB-) true)
      

;; Check that the function can-donate-to/cond? works as desired.
(check-expect (can-donate-to/cond? 'O- 'O-) true)
(check-expect (can-donate-to/cond? 'O- 'AB-) true)
(check-expect (can-donate-to/cond? 'O+ 'A-) false)
(check-expect (can-donate-to/cond? 'O+ 'B+) true)
(check-expect (can-donate-to/cond? 'A- 'B-) false)
(check-expect (can-donate-to/cond? 'A- 'A-) true)
(check-expect (can-donate-to/cond? 'A+ 'A-) false)
(check-expect (can-donate-to/cond? 'A+ 'AB+) true)
(check-expect (can-donate-to/cond? 'B- 'O+) false)
(check-expect (can-donate-to/cond? 'B- 'B+) true)
(check-expect (can-donate-to/cond? 'B+ 'AB-) false)
(check-expect (can-donate-to/cond? 'B+ 'AB+) true)
(check-expect (can-donate-to/cond? 'AB- 'A+) false)
(check-expect (can-donate-to/cond? 'AB- 'AB-) true)
(check-expect (can-donate-to/cond? 'AB+ 'B-) false)
(check-expect (can-donate-to/cond? 'A- 'A+) true)
(check-expect (can-donate-to/cond? 'O+ 'A+) true)
(check-expect (can-donate-to/cond? 'B- 'AB-) true)
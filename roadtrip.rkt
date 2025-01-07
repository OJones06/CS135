;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname roadtrip) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 02, Problem 5: "roadtrip.rkt"
;; ***************************************************
(define (city-number city)
  (cond
       [(symbol=? city 'StJohns) 1]
       [(symbol=? city 'Charlottetown) 2]
       [(symbol=? city 'Halifax) 3]
       [(symbol=? city 'Fredericton) 4]
       [(symbol=? city 'QuebecCity) 5]
       [(symbol=? city 'Toronto) 6]
       [(symbol=? city 'Waterloo) 7]
       [(symbol=? city 'SaultSteMarie) 8]
       [(symbol=? city 'ThunderBay) 9]
       [(symbol=? city 'Winnipeg) 10]
       [(symbol=? city 'Regina) 11]
       [(symbol=? city 'Calgary) 12]
       [(symbol=? city 'Vancouver) 13]))

#| travel-eligible? is a helper function that takes to natural
 numbers and determines the difference. If the difference
 is greater than 2 then it will return -1.
 travel-eligible?: Num Num -> Num |#
(define (travel-eligible? city-1 city-2)
  (cond
    [(and (>= 2 (- (city-number city-2) (city-number city-1)))
          (<= 0 (- (city-number city-2) (city-number city-1))))
     (- (city-number city-2) (city-number city-1))]
    [else -1]))

;; Seond-leg determines the fatigue level after the second leg, and sends it to the third leg.
;; second-leg: Symbol Symbol Symbol Num -> Num
(define (second-leg city-2 city-3 city-4 first-fatigue)
  (cond
     ;; ↓↓↓ If the length of the trip is longer than the fatigue left, the trip is invalid ↓↓↓
    [(> (travel-eligible? city-2 city-3) first-fatigue) -1]
    ;; ↓↓↓ If the length of next-trip is 2 and there is 2 fatigue left -> step 3 with 0 fatigue ↓↓↓
    [(= (travel-eligible? city-2 city-3) first-fatigue 2) (third-leg city-3 city-4 0)]
    ;; ↓↓↓ If the length of the next trip is 0, -> step 3 with one extra fatigue ↓↓↓
    ;; ↓↓↓ the minimum of (+ (travel-eligible? city-1 city-2) 1) and 2 ↓↓↓
    [(= (travel-eligible? city-2 city-3) 0) (third-leg city-3 city-4 (min (+ first-fatigue 1) 2))]
    ;; IF the length of the next trip is 1, then do step 3 with 1 fatigue
    [(= (travel-eligible? city-2 city-3) 1) (third-leg city-3 city-4 1)]))

;; Third-leg determines the fatigue level after the third leg, and sends the final fatigue to 'result'
;; third-leg: Symbol Symbol Num -> Num
(define (third-leg city-3 city-4 second-fatigue)
  (cond
    ;; ↓↓↓ If the length of the next trip is longer than the fatigue left, the trip is invalid ↓↓↓
    [(> (travel-eligible? city-3 city-4) second-fatigue) -1]
    ;; ↓↓↓ If the length of the trip is 0 add one to the fatigue capped at 2 ↓↓↓
    [(= (travel-eligible? city-3 city-4) 0) (min (+ second-fatigue 1) 2)]
    ;; ↓↓↓If the length of the trip is 1, finished with 1 ↓↓↓
    [(= (travel-eligible? city-3 city-4) 1) 1]
    ;; ↓↓↓ If the length of the trip is 2, finsihed with 0 ↓↓↓
    [(= (travel-eligible? city-3 city-4) 2) 0]))

;; Result concludes the final result to correspond with the driver's fatigue at the end of the trip
;; result: Num -> Symbol
(define (result final-fatigue)
  (cond
    [(= final-fatigue 2) 'rested]
    [(= final-fatigue 1) 'ready]
    [(= final-fatigue 0) 'exhausted]
    [(= final-fatigue -1) 'invalid]))

;; Check-plan is the function to be called by the user it will determine the driver's final fatigue.
;; check-plan: Symbol Symbol Symbol Symbol -> Symbol
(define (check-plan city-1 city-2 city-3 city-4)
  (cond
    ;; First checks to ensure that none of the 3 legs are invalid (> 2)
    [(= (travel-eligible? city-1 city-2) -1) 'invalid]
    [(= (travel-eligible? city-2 city-3) -1) 'invalid]
    [(= (travel-eligible? city-3 city-4) -1) 'invalid]
    ;; Checks the length of the first leg, then sends the remaining fatigue level to second-leg
    [(= (travel-eligible? city-1 city-2) 2) (result (second-leg city-2 city-3 city-4 0))]
    [(= (travel-eligible? city-1 city-2) 1) (result (second-leg city-2 city-3 city-4 1))]
    [(= (travel-eligible? city-1 city-2) 0) (result (second-leg city-2 city-3 city-4 2))]))

;; Check that the program works as intended.
(check-expect (check-plan 'Waterloo 'Waterloo 'SaultSteMarie 'ThunderBay) 'ready)
(check-expect (check-plan 'Regina 'Calgary 'Calgary 'Vancouver) 'ready)
(check-expect (check-plan 'Waterloo 'Waterloo 'Waterloo 'Waterloo) 'rested)
(check-expect (check-plan 'Regina 'Vancouver 'Vancouver 'Vancouver) 'rested)
(check-expect (check-plan 'SaultSteMarie 'SaultSteMarie 'ThunderBay 'ThunderBay) 'rested)
(check-expect (check-plan 'Halifax 'Fredericton 'Fredericton 'Toronto) 'exhausted)
(check-expect (check-plan 'Toronto 'Toronto 'Toronto 'SaultSteMarie) 'exhausted)
(check-expect (check-plan 'Regina 'Regina 'Regina 'Vancouver) 'exhausted)
(check-expect (check-plan 'StJohns 'Charlottetown 'Charlottetown 'Fredericton) 'exhausted)
(check-expect (check-plan 'SaultSteMarie 'SaultSteMarie 'ThunderBay 'Halifax) 'invalid)
(check-expect (check-plan 'Waterloo 'Toronto 'QuebecCity 'Fredericton) 'invalid)
(check-expect (check-plan 'Halifax 'Halifax 'Charlottetown 'Fredericton) 'invalid)
(check-expect (check-plan 'StJohns 'Halifax 'QuebecCity 'Waterloo) 'invalid)
(check-expect (check-plan 'Calgary 'Vancouver 'StJohns 'Charlottetown) 'invalid)
(check-expect (check-plan 'Winnipeg 'Winnipeg 'Calgary 'Calgary) 'ready)
(check-expect (check-plan 'Winnipeg 'Winnipeg 'Calgary 'Vancouver) 'invalid)
    
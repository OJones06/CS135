;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024)
;;   Assignment 01, Problem 3: "Functions"
;; ***************************************************

;; a) Manhattan distance:
(define (manhattan-distance x1 y1 x2 y2)
  ;|x1 - x2| + |y1 - y2|
  (+ (abs (- x1 x2)) (abs (- y1 y2)))) 
;; (manhattan-distance -3 9 5 -1) ; Testing if the program works correctly

;; b) Slugging average
(define (batter-slugging-average s d t hr ab)
  ;(s + 2d + 3t +4(hr)) / (ab)
  (/ (+ s (* 2 d) (* 3 t) (* 4 hr)) ab)) 
;; (better-slugging-average 2 8 4 6 2) ; Testing if the program works correctly

;; c) Cone area
(define (cone-area r h)
  ; π * r * (r + sqrt(h^2 +r^2))
  (* pi r (+ r (sqrt(+ (sqr h) (sqr r)))))) 
;; (cone-area 3 4) ; Testing if the program works correctly


;; d) Escape speed
(define G (* 6.674 (expt 10 -11)))
(define (escape M r)
  ; sqrt((2GM) / r)
  (sqrt (/ (* 2 G M) r)))
;; (escape 20000000 3) ; Testing if the program works correctly

;; e) The partition function
(define (partition-size-approximation n)
  ; 1 / (4n(sqrt(3))) * e^(pi * sqrt((2n) / 3))
  (* (/ 1 (* 4 n (sqrt 3))) (expt e (* pi (sqrt (/ (* 2 n) 3))))))
;; (partition-size-approximation 2) ; Testing if the program works correctly

;; f) Black Scholes formula
(define (d1 maturity rate volatility spot-price strike-price)
  (* (/ 1 (* volatility (sqrt maturity))) ; First part: 1 / (volatility * sqrt(maturity))
     (+ (log (/ spot-price strike-price) e) ; Second part: ln(spot-price / strike-price)
        (* (+ rate (/ (sqr volatility) 2)) maturity)))) ; Last part: rate + (volatility^2 / 2) * maturity
;; (d1 1 2 3 4 5) ; Testing if the program works correctly
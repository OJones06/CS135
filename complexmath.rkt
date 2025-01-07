;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname complexmath) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 03, Problem 2: "complexmath.rkt"
;; ***************************************************

;; A point is a (make-point Num Num)
(define-struct point (x y))

;; a)
;;    (point-mult point1 point2) consumes two Points and produces a Point that is the
;;    result of their multiplication as follows:
;;    (x1, y1)·(x2, y2) = (x1·x2-y1·y2, x1·y2 + x2·y1)
;; Examples:
(check-expect (point-mult (make-point 0 0) (make-point 0 0)) (make-point 0 0))
;; point-mult: Point Point -> Point
(define (point-mult point1 point2)
  (make-point
    (- (* (point-x point1) (point-x point2)) (* (point-y point1) (point-y point2))) ;x1·x2-y1·y2
    (+ (* (point-x point1) (point-y point2)) (* (point-x point2) (point-y point1))))) ;x1·y2 + x2·y1
;; Tests:
(check-expect (point-mult (make-point 5 0) (make-point 6 0)) (make-point 30 0))
(check-expect (point-mult (make-point -5 5) (make-point 2 4)) (make-point -30 -10))
(check-expect (point-mult (make-point 5 10) (make-point 15 20)) (make-point -125 250))
(check-expect (point-mult (make-point 1 2) (make-point 3 4)) (make-point -5 10))
(check-expect (point-mult (make-point 150 125) (make-point 234 910)) (make-point -78650 165750))

;; b)
;;    (point-div point1 point2) consumes two Points and produces a Point that is the
;;    result of their division as follows:
;;    (x1, y1)/(x2, y2) = ((x1·x2 + y1·y2)/(x2^2 + y2^2), (y1·x2-x1·y2)/(x2^2 + y2^2))
;; Examples:
(check-expect (point-div (make-point 3 4) (make-point 1 2)) (make-point 2.2 -0.4))
;; point-div: Point Point -> Point
(define (point-div point1 point2)
  (make-point
   (/ (+ (* (point-x point1) (point-x point2)) (* (point-y point1) (point-y point2))) ;(x1·x2 + y1·y2)
      (+ (expt (point-x point2) 2) (expt (point-y point2) 2))) ;(x2^2 + y2^2)
   (/ (- (* (point-y point1) (point-x point2)) (* (point-x point1) (point-y point2))) ;(y1·x2-x1·y2)
      (+ (expt (point-x point2) 2) (expt (point-y point2) 2))))) ;(x2^2 + y2^2)

;; Tests:
(check-expect (point-div (make-point 5 1) (make-point 2 3)) (make-point 1 -1))
(check-expect (point-div (make-point 7 -2) (make-point 4 6)) (make-point 4/13 -25/26))
(check-expect (point-div (make-point 8 3) (make-point 5 -1)) (make-point 37/26 23/26))
(check-expect (point-div (make-point 2 7) (make-point -3 4)) (make-point 22/25 -29/25))
(check-expect (point-div (make-point 0 5) (make-point 3 9)) (make-point 1/2 1/6))
(check-expect (point-div (make-point -4 3) (make-point 2 -5)) (make-point -23/29 -14/29))
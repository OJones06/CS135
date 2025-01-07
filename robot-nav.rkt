;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot-nav) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 03, Problem 3: "robot-nav.rkt"
;; ***************************************************

;; A Dir is anyof('North 'South 'East 'West)
;; A Comm is anyof('forward 'turn-left 'turn-right)


;; A state is a Num Num Dir
(define-struct state (x y dir))


;; (move-forward point1) consumes a State and produces a State where the value of the x or y is
;; the result of moving the robot 1 in dir with a min of 0 and a max of 1.

;; Examples:
(check-expect (move-forward (make-state 10 10 'North)) (make-state 10 10 'North))
(check-expect (move-forward (make-state 5 5 'West)) (make-state 4 5 'West))
(check-expect (move-forward (make-state 9 10 'East)) (make-state 10 10 'East))

;; move-forward: State -> State

(define (move-forward point1)
  (cond
    [(symbol=? (state-dir point1) 'North) (make-state
                                    (state-x point1)
                                    (min (+ 1 (state-y point1)) 10)
                                    (state-dir point1))]
    [(symbol=? (state-dir point1) 'South) (make-state
                                    (state-x point1)
                                    (max (+ -1 (state-y point1)) 0)
                                    (state-dir point1))]
    [(symbol=? (state-dir point1) 'East) (make-state
                                    (min (+ 1 (state-x point1)) 10)
                                    (state-y point1)
                                    (state-dir point1))]
    [(symbol=? (state-dir point1) 'West) (make-state
                                    (max (+ -1 (state-x point1)) 0)
                                    (state-y point1)
                                    (state-dir point1))]
    ))

;; (turn-left point1) consumes a State and produces a State where the dir of the State is the
;; result of the robot turning left.

;; Examples:
(check-expect (turn-left (make-state 3 7 'West)) (make-state 3 7 'South))
(check-expect (turn-left (make-state 10 0 'South)) (make-state 10 0 'East))
(check-expect (turn-left (make-state 10 10 'East)) (make-state 10 10 'North))

;; turn-left State -> State

(define (turn-left point1)
  (cond
    [(symbol=? (state-dir point1) 'North) (make-state
                                    (state-x point1)
                                    (state-y point1)
                                    'West)]
    [(symbol=? (state-dir point1) 'South) (make-state
                                    (state-x point1)
                                    (state-y point1)
                                    'East)]
    [(symbol=? (state-dir point1) 'East) (make-state
                                    (state-x point1)
                                    (state-y point1)
                                    'North)]
    [(symbol=? (state-dir point1) 'West) (make-state
                                    (state-x point1)
                                    (state-y point1)
                                    'South)]
    ))


;; (turn-right point1) consumes a State and produces a State where the dir of the State is the
;; result of the robot turning right.

;; Examples:
(check-expect (turn-right (make-state 3 7 'West)) (make-state 3 7 'North))
(check-expect (turn-right (make-state 10 0 'South)) (make-state 10 0 'West))
(check-expect (turn-right (make-state 10 10 'East)) (make-state 10 10 'South))

;; turn-right State -> State
(define (turn-right point1)
  (cond
    [(symbol=? (state-dir point1) 'North) (make-state
                                    (state-x point1)
                                    (state-y point1)
                                    'East)]
    [(symbol=? (state-dir point1) 'South) (make-state
                                    (state-x point1)
                                    (state-y point1)
                                    'West)]
    [(symbol=? (state-dir point1) 'East) (make-state
                                    (state-x point1)
                                    (state-y point1)
                                    'South)]
    [(symbol=? (state-dir point1) 'West) (make-state
                                    (state-x point1)
                                    (state-y point1)
                                    'North)]
    ))
                                   

;; (robot-ctl state command) consumes two Nums (x y) as well as a Dir that represent the state of the
;; robot and produces two Nums (x y) as well as a Dir that represent the final state of the robot.
;; robot-ctl: State Comm -> State

;; Examples:
(check-expect (robot-ctl (make-state 3 7 'West) 'turn-left) (make-state 3 7 'South))
(check-expect (robot-ctl (make-state 10 0 'South) 'turn-right) (make-state 10 0 'West))
(check-expect (robot-ctl (make-state 10 10 'South) 'forward) (make-state 10 9 'South))

;; robot-ctl: State Comm -> State
(define (robot-ctl state command)
  (cond
    [(symbol=? command 'forward) (move-forward state)] ; command is 'Forward
    [(symbol=? command 'turn-left) (turn-left state)] ; command is 'turn-left
    [(symbol=? command 'turn-right) (turn-right state)])) ; command is 'turn-right



(check-expect (robot-ctl (make-state 2 8 'North) 'turn-right) (make-state 2 8 'East))
(check-expect (robot-ctl (make-state 10 10 'North) 'forward) (make-state 10 10 'North))
(check-expect (robot-ctl (make-state 0 0 'North) 'turn-left) (make-state 0 0 'West))
(check-expect (robot-ctl (make-state 5 5 'West) 'forward) (make-state 4 5 'West))
(check-expect (robot-ctl (make-state 9 10 'East) 'forward) (make-state 10 10 'East))
(check-expect (robot-ctl (make-state 2 4 'South) 'turn-left) (make-state 2 4 'East))
(check-expect (robot-ctl (make-state 4 2 'East) 'turn-right) (make-state 4 2 'South))
(check-expect (robot-ctl (make-state 0 0 'North) 'forward) (make-state 0 1 'North))
(check-expect (robot-ctl (make-state 7 1 'West) 'turn-right) (make-state 7 1 'North))
(check-expect (robot-ctl (make-state 1 10 'West) 'forward) (make-state 0 10 'West))
(check-expect (robot-ctl (make-state 8 0 'East) 'turn-left) (make-state 8 0 'North))
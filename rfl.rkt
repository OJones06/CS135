;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rfl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 03, Problem 4: "rfl.rkt"
;; ***************************************************

;; a)

;; A Sub-mod is a Str Num
(define-struct sub-mod (name weight))

;; A Mod is a Str Num Sub-mod Sub-mod
(define-struct mod (name frame-wt primary secondary))

;; A Robot is a Str Mod Mod Mod
(define-struct robot (name head arms legs))

;; b)
;; (mod-weight module) consumes a Mod and produces the total weight of the Mod
;; Examples:
(check-expect (mod-weight (make-mod "Flamethrower" 10
                                    (make-sub-mod "Ignition" 20)
                                    (make-sub-mod "Fuel container" 30))) 60)
(check-expect (mod-weight (make-mod "Hammer" 599
                                    (make-sub-mod "Handle" 1)
                                    (make-sub-mod "Head" 25))) 625)
;; mod-weight: Mod -> Num
(define (mod-weight module)
  (+ (mod-frame-wt module) ; Add the frame weight
     (sub-mod-weight (mod-primary module)) ; Add the primary sub-mod's weight
     (sub-mod-weight (mod-secondary module)) ; Add the secondary sub-mod's weight
     ))

;; Tests:
(check-expect (mod-weight (make-mod "Gun" 10
                                    (make-sub-mod "Body" 100)
                                    (make-sub-mod "Bullets" 6))) 116)
(check-expect (mod-weight (make-mod "Head" 1670
                                    (make-sub-mod "Skin" 177)
                                    (make-sub-mod "Skull" 177))) 2024)
(check-expect (mod-weight (make-mod "Wheels" 1
                                    (make-sub-mod "Wheel-1" 1)
                                    (make-sub-mod "Wheel-2" 1))) 3)
(check-expect (mod-weight (make-mod "Wings" 17
                                    (make-sub-mod "Right-wing" 34)
                                    (make-sub-mod "Left-wing" 34))) 85)
(check-expect (mod-weight (make-mod "Rocket" 100000000
                                    (make-sub-mod "Jimmy" 30)
                                    (make-sub-mod "Darcy" 20))) 100000050)
(check-expect (mod-weight (make-mod "Battery" 11
                                    (make-sub-mod "Case" 22)
                                    (make-sub-mod "Metal" 33))) 66)


;; c)
;; (robot-weight robot) consumes a Robot and produces the total weight of the robot
;; Examples:
(check-expect (robot-weight (make-robot "Jimmy"
                                          (make-mod "Head-S" 50
                                                    (make-sub-mod "Eye-S" 20)
                                                    (make-sub-mod "Eye-S" 20))
                                          (make-mod "Arms-S" 50
                                                    (make-sub-mod "Fist-S" 30)
                                                    (make-sub-mod "Fist-S" 30))
                                          (make-mod "Legs-S" 50
                                                    (make-sub-mod "Foot-S-Big" 25)
                                                    (make-sub-mod "Foot-S-Small" 25))))
                            300)
(check-expect (robot-weight (make-robot "Smasher"
                                          (make-mod "Head-S" 30
                                                    (make-sub-mod "Eye-S" 10)
                                                    (make-sub-mod "Eye-S" 10))
                                          (make-mod "Arms-S" 50
                                                    (make-sub-mod "Fist-S" 15)
                                                    (make-sub-mod "Fist-S" 15))
                                          (make-mod "Legs-S" 30
                                                    (make-sub-mod "Foot-S-Big" 8)
                                                    (make-sub-mod "Foot-S-Small" 5))))
              173)
;; robot-weight: Robot -> Num
(define (robot-weight robot)             
  (+ (mod-weight (robot-head robot)) ; Add the weight of the head (first module)
     (mod-weight (robot-arms robot)) ; Add the weight of the arms (second module)
     (mod-weight (robot-legs robot)) ; Add the weight of the legs (third module)
     ))

;; (predict-winner robot-1 robot-2) consumes two Robots and produces the name of
;; the robot with the greater weight.
;; Examples:
(check-expect (predict-winner (make-robot "Smasher"
                                          (make-mod "Head-S" 30
                                                    (make-sub-mod "Eye-S" 10)
                                                    (make-sub-mod "Eye-S" 10))
                                          (make-mod "Arms-S" 50
                                                    (make-sub-mod "Fist-S" 15)
                                                    (make-sub-mod "Fist-S" 15))
                                          (make-mod "Legs-S" 30
                                                    (make-sub-mod "Foot-S-Big" 8)
                                                    (make-sub-mod "Foot-S-Small" 5)))
                              (make-robot "Jimmy"
                                          (make-mod "Head-S" 50
                                                    (make-sub-mod "Eye-S" 20)
                                                    (make-sub-mod "Eye-S" 20))
                                          (make-mod "Arms-S" 50
                                                    (make-sub-mod "Fist-S" 30)
                                                    (make-sub-mod "Fist-S" 30))
                                          (make-mod "Legs-S" 50
                                                    (make-sub-mod "Foot-S-Big" 25)
                                                    (make-sub-mod "Foot-S-Small" 25))))
              "Jimmy")

;; predict-winner: Robot Robot -> Str
(define (predict-winner robot-1 robot-2)
  (cond
    [(>= (robot-weight robot-1) (robot-weight robot-2)) (robot-name robot-1)] ; Robot one is heavier
    [else (robot-name robot-2)] ; Robot two is heavier
    ))


(check-expect (predict-winner (make-robot "Smasher"
                                          (make-mod "Head-S" 30
                                                    (make-sub-mod "Eye-S" 10)
                                                    (make-sub-mod "Eye-S" 10))
                                          (make-mod "Arms-S" 50
                                                    (make-sub-mod "Fist-S" 15)
                                                    (make-sub-mod "Fist-S" 15))
                                          (make-mod "Legs-S" 30
                                                    (make-sub-mod "Foot-S-Big" 8)
                                                    (make-sub-mod "Foot-S-Small" 5)))
                              (make-robot "Jimmy"
                                          (make-mod "Head-S" 50
                                                    (make-sub-mod "Eye-S" 20)
                                                    (make-sub-mod "Eye-S" 20))
                                          (make-mod "Arms-S" 50
                                                    (make-sub-mod "Fist-S" 30)
                                                    (make-sub-mod "Fist-S" 30))
                                          (make-mod "Legs-S" 50
                                                    (make-sub-mod "Foot-S-Big" 25)
                                                    (make-sub-mod "Foot-S-Small" 25))))
              "Jimmy")

(check-expect (predict-winner (make-robot "Bill Nye"
                                          (make-mod "Head" 1670
                                                    (make-sub-mod "Skin" 177)
                                                    (make-sub-mod "Skull" 177))
                                          (make-mod "Gun" 10
                                                    (make-sub-mod "Body" 100)
                                                    (make-sub-mod "Bullets" 6))
                                          (make-mod "Rocket" 100000000
                                                    (make-sub-mod "Jimmy" 30)
                                                    (make-sub-mod "Darcy" 20)))
                              (make-robot "Allie"
                                          (make-mod "Battery" 11
                                                    (make-sub-mod "Case" 22)
                                                    (make-sub-mod "Metal" 33))
                                          (make-mod "Wings" 17
                                                    (make-sub-mod "Right-wing" 34)
                                                    (make-sub-mod "Left-wing" 34))
                                          (make-mod "Wheels" 1
                                                    (make-sub-mod "Wheel-1" 1)
                                                    (make-sub-mod "Wheel-2" 1))))
              "Bill Nye")

(check-expect (predict-winner (make-robot "Steve"
                                          (make-mod "Head" 1670
                                                    (make-sub-mod "Skin" 177)
                                                    (make-sub-mod "Skull" 177))
                                          (make-mod "Gun" 10
                                                    (make-sub-mod "Body" 100)
                                                    (make-sub-mod "Bullets" 6))
                                          (make-mod "Rocket" 100000000
                                                    (make-sub-mod "Jimmy" 30)
                                                    (make-sub-mod "Darcy" 20)))
                              (make-robot "Alex"
                                          (make-mod "Head" 1670
                                                    (make-sub-mod "Skin" 177)
                                                    (make-sub-mod "Skull" 177))
                                          (make-mod "Gun" 10
                                                    (make-sub-mod "Body" 100)
                                                    (make-sub-mod "Bullets" 6))
                                          (make-mod "Rocket" 100000000
                                                    (make-sub-mod "Jimmy" 30)
                                                    (make-sub-mod "Darcy" 20))))
              "Steve")

(check-expect (predict-winner (make-robot "Jesus"
                                          (make-mod "Way" 1000
                                                    (make-sub-mod "Cross" 150)
                                                    (make-sub-mod "Grave" 150))
                                          (make-mod "Truth" 1000
                                                    (make-sub-mod "Love" 50)
                                                    (make-sub-mod "Honesty" 50))
                                          (make-mod "Life" 1000
                                                    (make-sub-mod "Resurrection" 500)
                                                    (make-sub-mod "Ascension" 500)))
                              (make-robot "Satan"
                                          (make-mod "Evil" 1
                                                    (make-sub-mod "L1" 1)
                                                    (make-sub-mod "L2" 1))
                                          (make-mod "Jealous" 1
                                                    (make-sub-mod "L3" 1)
                                                    (make-sub-mod "L4" 1))
                                          (make-mod "Eternity" 1
                                                    (make-sub-mod "L5" 1)
                                                    (make-sub-mod "L6" 1))))
              "Jesus") ; Spoiler alert. Jesus wins!

;; d)
;; (max-sub-mod-4 sub1 sub2 sub3 sub4) consumes 4 Sub-mods and produces the weight of the best Sub-mod
;; Example:
(check-expect (max-sub-mod-4
               (make-sub-mod "Ears-W" 150)
               (make-sub-mod "Eyes-W" 100) 
               (make-sub-mod "Fingers-W" 50)
               (make-sub-mod "Hands-W" 150))
              150)
;; max-sub-mod-4: Sub-mod Sub-mod Sub-mod Sub-mod -> Num              
(define (max-sub-mod-4 sub1 sub2 sub3 sub4)
  (max (sub-mod-weight sub1) (sub-mod-weight sub2) (sub-mod-weight sub3) (sub-mod-weight sub4)))

;; (max-sub-mod-3 sub1 sub2 sub3 sub4) consumes 4 Sub-mods and considers Sub-mods 2-4,
;; producing the weight of the best Sub-mod
;; Example:
(check-expect (max-sub-mod-3
               (make-sub-mod "Ears-W" 150)
               (make-sub-mod "Eyes-W" 25) 
               (make-sub-mod "Fingers-W" 100)
               (make-sub-mod "Hands-W" 75))
              100)
;; max-sub-mod-3: Sub-mod Sub-mod Sub-mod Sub-mod -> Num  
(define (max-sub-mod-3 sub1 sub2 sub3 sub4)
  (max (sub-mod-weight sub2) (sub-mod-weight sub3) (sub-mod-weight sub4)))

;; (best-sub-mod sub1 sub2 sub3 sub4) consumes 4 Sub-mods and produces a duplicate of the
;; best Sub-mod
;; Example:
(check-expect (best-sub-mod
               (make-sub-mod "Ears-W" 150)
               (make-sub-mod "Eyes-W" 25) 
               (make-sub-mod "Fingers-W" 100)
               (make-sub-mod "Hands-W" 75))
              (make-sub-mod "Ears-W" 150))
;; best-sub-mod: Sub-mod Sub-mod Sub-mod Sub-mod -> Sub-mod
(define (best-sub-mod sub1 sub2 sub3 sub4) ; find the best sub mod
  (cond
    [(= (max-sub-mod-4 sub1 sub2 sub3 sub4) (sub-mod-weight sub1))
     (make-sub-mod
      (sub-mod-name sub1)
      (sub-mod-weight sub1))] ; sub1 is the best
    [(= (max-sub-mod-4 sub1 sub2 sub3 sub4) (sub-mod-weight sub2))
     (make-sub-mod
      (sub-mod-name sub2)
      (sub-mod-weight sub2))] ; sub2 is the best
    [(= (max-sub-mod-4 sub1 sub2 sub3 sub4) (sub-mod-weight sub3))
     (make-sub-mod
      (sub-mod-name sub3)
      (sub-mod-weight sub3))] ; sub3 is the best
    [(= (max-sub-mod-4 sub1 sub2 sub3 sub4) (sub-mod-weight sub4))
     (make-sub-mod
      (sub-mod-name sub4)
      (sub-mod-weight sub4))] ; sub4 is the best
    ))

;; (secoond-best-sub-mod sub1 sub2 sub3 sub4) consumes 4 Sub-mods and produces a duplicate of the
;; second best Sub-mod
;; Example:
(check-expect (second-best-sub-mod
               (make-sub-mod "Ears-W" 150)
               (make-sub-mod "Eyes-W" 25) 
               (make-sub-mod "Fingers-W" 100)
               (make-sub-mod "Hands-W" 75))
              (make-sub-mod "Fingers-W" 100))
;; second-best-sub-mod: Sub-mod Sub-mod Sub-mod Sub-mod -> Sub-mod
(define (second-best-sub-mod sub1 sub2 sub3 sub4) ; find the best sub mod
  (cond
    [(= (max-sub-mod-4 sub1 sub2 sub3 sub4) (sub-mod-weight sub1)) ; sub1 is the best
     (cond
       [(= (max-sub-mod-3 sub1 sub2 sub3 sub4) (sub-mod-weight sub2)) ; sub2 is the second best
        (make-sub-mod
         (sub-mod-name sub2)
         (sub-mod-weight sub2))]
       [(= (max-sub-mod-3 sub1 sub2 sub3 sub4) (sub-mod-weight sub3)) ; sub3 is the second best
        (make-sub-mod
         (sub-mod-name sub3)
         (sub-mod-weight sub3))]
       [(= (max-sub-mod-3 sub1 sub2 sub3 sub4) (sub-mod-weight sub4)) ; sub4 is the second best
        (make-sub-mod
         (sub-mod-name sub4)
         (sub-mod-weight sub4))])]
    [(= (max-sub-mod-4 sub1 sub2 sub3 sub4) (sub-mod-weight sub2)) ; sub2 is the best
     (cond
       [(= (max-sub-mod-3 sub2 sub1 sub3 sub4) (sub-mod-weight sub1)) ; sub1 is the second best
        (make-sub-mod
         (sub-mod-name sub1)
         (sub-mod-weight sub1))]
       [(= (max-sub-mod-3 sub2 sub1 sub3 sub4) (sub-mod-weight sub3)) ; sub3 is the second best
        (make-sub-mod
         (sub-mod-name sub3)
         (sub-mod-weight sub3))]
       [(= (max-sub-mod-3 sub2 sub1 sub3 sub4) (sub-mod-weight sub4)) ; sub4 is the second best
        (make-sub-mod
         (sub-mod-name sub4)
         (sub-mod-weight sub4))])]
    [(= (max-sub-mod-4 sub1 sub2 sub3 sub4) (sub-mod-weight sub3)) ; sub3 is the best
     (cond
       [(= (max-sub-mod-3 sub3 sub1 sub2 sub4) (sub-mod-weight sub1)) ; sub1 is the second best
        (make-sub-mod
         (sub-mod-name sub1)
         (sub-mod-weight sub1))]
       [(= (max-sub-mod-3 sub3 sub1 sub2 sub4) (sub-mod-weight sub2)) ; sub2 is the second best
        (make-sub-mod
         (sub-mod-name sub2)
         (sub-mod-weight sub2))]
       [(= (max-sub-mod-3 sub3 sub1 sub2 sub4) (sub-mod-weight sub4)) ; sub4 is the second best
        (make-sub-mod
         (sub-mod-name sub4)
         (sub-mod-weight sub4))])]
    [(= (max-sub-mod-4 sub1 sub2 sub3 sub4) (sub-mod-weight sub4)) ; sub4 is the best
     (cond
       [(= (max-sub-mod-3 sub4 sub1 sub2 sub3) (sub-mod-weight sub1)) ; sub1 is the second best
        (make-sub-mod
         (sub-mod-name sub1)
         (sub-mod-weight sub1))]
       [(= (max-sub-mod-3 sub4 sub1 sub2 sub3) (sub-mod-weight sub2)) ; sub2 is the second best
        (make-sub-mod
         (sub-mod-name sub2)
         (sub-mod-weight sub2))]
       [(= (max-sub-mod-3 sub4 sub1 sub2 sub3) (sub-mod-weight sub3)) ; sub4 is the second best
        (make-sub-mod
         (sub-mod-name sub3)
         (sub-mod-weight sub3))])]
    ))

;; (mod-compare mod1 mod2) consumes 2 Mods and produces a duplicate of the best Mod,
;; with Sub-mods to be found by the best-sub-mod and second-best-sub-mod functions.
;; Examples:
(check-expect (mod-compare
              (make-mod "Head-L" 200
                        (make-sub-mod "Ears-L" 15)
                        (make-sub-mod "Eyes-L" 15))
              (make-mod "Head-B" 250
                        (make-sub-mod "Ears-B" 5)
                        (make-sub-mod "Eyes-B" 25)))
             (make-mod "Head-B" 250
                       (make-sub-mod "Eyes-B" 25)
                       (make-sub-mod "Ears-L" 15)))
;; mod-compare: Mod Mod -> Mod
(define (mod-compare mod1 mod2)
  (cond
    [(>= (mod-frame-wt mod1) (mod-frame-wt mod2)) ; Best frame is mod1
     (make-mod
      (mod-name mod1) ; name of mod 1
      (mod-frame-wt mod1)
      (best-sub-mod (mod-primary mod1) ; find best sub-mod
                (mod-secondary mod1)
                (mod-primary mod2)
                (mod-secondary mod2))
      (second-best-sub-mod (mod-primary mod1) ; find second best sub-mod
                (mod-secondary mod1)
                (mod-primary mod2)
                (mod-secondary mod2))
      )]
    [else
     (make-mod
      (mod-name mod2) ; name of mod 2
      (mod-frame-wt mod2)
      (best-sub-mod (mod-primary mod2) ; find best sub-mod
                    (mod-secondary mod2)
                    (mod-primary mod1)
                    (mod-secondary mod1))
      (second-best-sub-mod (mod-primary mod2) ; find second best sub-mod
                           (mod-secondary mod2)
                           (mod-primary mod1)
                           (mod-secondary mod1))
      )]
    ))
;; (combine robot-1 robot-2) consumes 2 Robots and produces the best possible combination of
;; the robot if you could switch out the modules.
;; Examples:
(check-expect (combine
               (make-robot "Winston"
                           (make-mod "Head-W" 400
                                     (make-sub-mod "Ears-W" 150)
                                     (make-sub-mod "Eyes-W" 100))
                           (make-mod "Arms-W" 160
                                     (make-sub-mod "Fingers-W" 50)
                                     (make-sub-mod "Hands-W" 150))
                           (make-mod "Legs-W" 125
                                     (make-sub-mod "Feet-W" 50)
                                     (make-sub-mod "Toes-W" 25)))
               (make-robot "Maria"
                           (make-mod "Head-M" 490
                                     (make-sub-mod "Ears-M" 155)
                                     (make-sub-mod "Eyes-M" 190))
                           (make-mod "Arms-M" 350
                                     (make-sub-mod "Fingers-M" 160)
                                     (make-sub-mod "Hands-M" 155))
                           (make-mod "Legs-M" 250
                                     (make-sub-mod "Feet-M" 150)
                                     (make-sub-mod "Toes-M" 55))))
              (make-robot "Winston-Maria"
                          (make-mod "Head-M" 490
                                    (make-sub-mod "Eyes-M" 190)
                                    (make-sub-mod "Ears-M" 155))
                          (make-mod "Arms-M" 350
                                    (make-sub-mod "Fingers-M" 160)
                                    (make-sub-mod "Hands-M" 155))
                          (make-mod "Legs-M" 250
                                    (make-sub-mod "Feet-M" 150)
                                    (make-sub-mod "Toes-M" 55))))
;; combine: Robot Robot -> Robot
(define (combine robot-1 robot-2)
  (make-robot
   (string-append (robot-name robot-1) "-" (robot-name robot-2))
   (mod-compare (robot-head robot-1) (robot-head robot-2))
   (mod-compare (robot-arms robot-1) (robot-arms robot-2))
   (mod-compare (robot-legs robot-1) (robot-legs robot-2))
   ))

(check-expect (combine (make-robot "Brock"
                                   (make-mod "Head-B" 15
                                             (make-sub-mod "Ears-B" 5)
                                             (make-sub-mod "Eyes-B" 5))
                                   (make-mod "Arms-B" 170
                                             (make-sub-mod "Fingers-B" 10)
                                             (make-sub-mod "Hands-B" 10))
                                   (make-mod "Legs-B" 175
                                             (make-sub-mod "Feet-B" 15)
                                             (make-sub-mod "Toes-B" 15)))
                       (make-robot "Lola"
                                   (make-mod "Head-L" 200
                                             (make-sub-mod "Ears-L" 15)
                                             (make-sub-mod "Eyes-L" 15))
                                   (make-mod "Arms-L" 50
                                             (make-sub-mod "Fingers-L" 11)
                                             (make-sub-mod "Hands-L" 9))
                                   (make-mod "Legs-L" 100
                                             (make-sub-mod "Feet-L" 5)
                                             (make-sub-mod "Toes-L" 5))))
              (make-robot "Brock-Lola"
                                   (make-mod "Head-L" 200
                                             (make-sub-mod "Ears-L" 15)
                                             (make-sub-mod "Eyes-L" 15))
                                   (make-mod "Arms-B" 170
                                             (make-sub-mod "Fingers-L" 11)
                                             (make-sub-mod "Fingers-B" 10))
                                   (make-mod "Legs-B" 175
                                             (make-sub-mod "Feet-B" 15)
                                             (make-sub-mod "Toes-B" 15))))

(check-expect (combine (make-robot "Alex"
                                   (make-mod "Head-A" 10
                                             (make-sub-mod "Eye-A" 5)
                                             (make-sub-mod "Eye-A" 5))
                                   (make-mod "Arms-A" 100
                                             (make-sub-mod "Fist-A-Big" 50)
                                             (make-sub-mod "Fist-A-Small" 2))
                                   (make-mod "Legs-A" 20
                                             (make-sub-mod "Foot-A" 10)
                                             (make-sub-mod "Foot-A" 10)))
                       (make-robot "Sandy"
                                   (make-mod "Head-S" 30
                                             (make-sub-mod "Eye-S" 10)
                                             (make-sub-mod "Eye-S" 10))
                                   (make-mod "Arms-S" 50
                                             (make-sub-mod "Fist-S" 15)
                                             (make-sub-mod "Fist-S" 15))
                                   (make-mod "Legs-S" 30
                                             (make-sub-mod "Foot-S-Big" 8)
                                             (make-sub-mod "Foot-S-Small" 5))))
              (make-robot "Alex-Sandy"
                          (make-mod "Head-S" 30
                                    (make-sub-mod "Eye-S" 10)
                                    (make-sub-mod "Eye-S" 10))
                          (make-mod "Arms-A" 100
                                    (make-sub-mod "Fist-A-Big" 50)
                                    (make-sub-mod "Fist-S" 15))
                          (make-mod "Legs-S" 30
                                    (make-sub-mod "Foot-A" 10)
                                    (make-sub-mod "Foot-A" 10))))
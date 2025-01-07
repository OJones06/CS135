;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname components) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 07 Problem 1: "components.rkt"
;; ***************************************************

;; Q1: "components.rkt"
;; mutual recursion
(define-struct component (name num subcomponents))
;; A Component is a (make-component Str Nat (listof Component))

;; (contains-component? component name) consumes a Component and a Str and produces true if the
;; Str is a name in the Component, and false otherwise.
;; Example:
(check-expect (contains-component? sword "blade") true) ; sword is defined below.
;; contains-component?: Component Str -> Bool
(define (contains-component? component name)
  (cond
    [(empty? (component-subcomponents component)) false]
    [(contains-sub-component? (first (component-subcomponents component)) name) true]
    [else (contains-component? (make-component
                                (component-name component)
                                (component-num component)
                                (rest (component-subcomponents component))) name)]))

;; (contains-sub-component? subcomponent name) consumes a Component and produces true if the
;; name is in the subcomponent and false otherwise. It is used mutually recursively with
;; the function "contains-component?"
;; Example:
(check-expect (contains-sub-component? (make-component "tire" 1 empty) "tire") true)
;; contains-sub-component? Component Str -> Bool
(define (contains-sub-component? subcomponent name)
  (cond
    [(string=? name (component-name subcomponent)) true]
    [(empty? (component-subcomponents subcomponent)) false]
    [else
     (contains-component? (make-component
                           (component-name subcomponent)
                           (component-num subcomponent)
                           (component-subcomponents subcomponent)) name)]))

;; Tests:

(define bike
  (make-component
   "bike" 1
   (list
    (make-component "frame" 1 empty)
    (make-component "wheel" 2 (list
                               (make-component "tire" 1 empty)
                               (make-component "rim" 1 empty)
                               (make-component "spoke" 30 empty)
                               (make-component "hub" 1 (list
                                                        (make-component "housing" 1 empty)
                                                        (make-component "axel" 1 empty)
                                                        (make-component "bearing" 20 empty)))))
    (make-component "seat" 1 empty)
    (make-component "handlebar" 1 empty))))

(define sword (make-component "sword" 1
                              (list
                               (make-component "handle" 1
                                               (list (make-component "wood" 1 empty)))
                               (make-component "blade" 1
                                               (list (make-component "iron" 1 empty))))))



(check-expect (contains-component? sword "wood") true)
(check-expect (contains-component? sword "stone") false)

(check-expect (contains-component? bike "hub") true)
(check-expect (contains-component? bike "brake") false)

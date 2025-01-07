;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024)
;;   Assignment 01, Problem 6: "Grades"
;; ***************************************************

;; Class participation is worth 5%: 5/100
;; Midterm Exam is worth 25%: 25/100
;; Final Exam is worth 45%: 45/100
;; Assignments are worth 25%: 25/100 

;; cs: class participation, mt: midterm, fe: final exam, as: assignment grade.
(define (final-cs135-grade cp mt fe as)
  (+ (* cp 5/100) (* mt 25/100) (* fe 45/100) (* as 25/100)))
;; (final-cs135-grade 100 100 0 100) Testing if the program works correctly

(define (cs135-final-exam-grade-needed cp mt as)
  ;; (60% - Class mark before the exam) * (recipricol of Final Exam worth) = Exam percentage needed
  (* (- 60 (+ (* cp 5/100) (* mt 25/100) (* as 25/100))) 100/45))

;; (cs135-final-exam-grade-needed 100 100 0) Testing if the program works correctly
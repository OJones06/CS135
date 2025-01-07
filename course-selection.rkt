;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname course-selection) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 05, Problem 2 "course-selection.rkt"
;; ***************************************************

;; A DesiredCourses is one of:
;; * empty
;; * (cons (list Str (listof Sym)) DesiredCourses)

(define selections
  (list
   (list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
   (list "w46dles" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
   (list "d32pines" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
   (list "gnclstan" (list 'ANTH241 'LS201 'AMATH231 'PMATH347))))

;; a) missed-deadline-add
;; (in-list? list str) consumes a list of lists of Str and a string and produces true if the string
;; is in the list and false otherwise.
;; Example:
(check-expect (in-list? (list (list "1") (list "2") (list "3")) "3") true)
;; in-list?: List Str -> Bool
(define (in-list? list str)
  (cond
    [(empty? list) false]
    [(string=? (first (first list)) str) true]
    [else (in-list? (rest list) str)]))
;; (missed-deadline-add selections studentID) consumes a DesiredCourses and a studentID (Str)
;; and adds the studentID to the end of the list if it is not in the list and produces the
;; original list otherwise.
;; Example:
(check-expect (missed-deadline-add
               empty "OJones") (list (list "OJones")))
;; missed-deadline-add: DesiredCourses Str -> List
(define (missed-deadline-add selections studentID)
  (cond
    [(in-list? selections studentID) selections]
    [(empty? selections) (list (list studentID))]
    [else (cons (first selections) (missed-deadline-add (rest selections) studentID))]))
;; Tests:
(check-expect (missed-deadline-add
               (list
                (list "Clay" (list 'CS135))
                (list "Steph" (list 'Math115 'CS115 'COMMST100 'PHYS111 'MATH117))
                (list "Lebron" (list 'PHYS121 'PHYS122 'MATH145 'MATH147 'PD1))
                (list "James" (list 'CS145 'MATH147 'MATH145 'COMMST236))) "OJones")
              (list
                (list "Clay" (list 'CS135))
                (list "Steph" (list 'Math115 'CS115 'COMMST100 'PHYS111 'MATH117))
                (list "Lebron" (list 'PHYS121 'PHYS122 'MATH145 'MATH147 'PD1))
                (list "James" (list 'CS145 'MATH147 'MATH145 'COMMST236))
                (list "OJones")))
(check-expect (missed-deadline-add
               (list
                (list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
                (list "w46dles" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
                (list "d32pines" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
                (list "gnclstan" (list 'ANTH241 'LS201 'AMATH231 'PMATH347))) "mpines")
               (list
                (list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
                (list "w46dles" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
                (list "d32pines" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
                (list "gnclstan" (list 'ANTH241 'LS201 'AMATH231 'PMATH347))))


;; b) taking-course?
;; (user-taking-course? user-couses course) consumes a listof courses and a courses and produces
;; true if the course is in the list and false otherwise.
;; Example:
(check-expect (user-taking-course?
               (list 'CS145 'MATH145 'COMMST100) 'MATH145) true)
;; user-taking-course?: List Str -> Bool
(define (user-taking-course? user-courses course)
  (cond
    [(empty? user-courses) false]
    [(symbol=? (first user-courses) course) true]
    [else (user-taking-course? (rest user-courses) course)]))

;; (taking-course? selections quest-user course) consumes a DesiredCourses, a student's Quest username
;; and a course code, and produces true if the student has selected the course and false otherwise.
;; Example:
(check-expect (taking-course?
               (list
                (list "Clay" (list 'CS135))
                (list "Steph" (list 'Math115 'CS115 'COMMST100 'PHYS111 'MATH117))
                (list "Lebron" (list 'PHYS121 'PHYS122 'MATH145 'MATH147 'PD1))
                (list "James" (list 'CS145 'MATH147 'MATH145 'COMMST236))) "Steph" 'COMMST100)
              true)
;; taking-course?: DesiredCourses Str Sym -> Bool
(define (taking-course? selections quest-user course)
  (cond
    [(empty? selections) false]
    [(string=? (first (first selections)) quest-user)
     (user-taking-course? (second (first selections)) course)]
    [else (taking-course? (rest selections) quest-user course)]))
;; Tests: 
(check-expect (taking-course?
               (list
                (list "Clay" (list 'CS135))
                (list "Steph" (list 'Math115 'CS115 'COMMST100 'PHYS111 'MATH117))
                (list "Lebron" (list 'PHYS121 'PHYS122 'MATH145 'MATH147 'PD1))
                (list "James" (list 'CS145 'MATH147 'MATH145 'COMMST236))) "James" 'COMMST100)
              false)
(check-expect (taking-course?
               (list
                (list "Clay" (list 'CS135))
                (list "Steph" (list 'Math115 'CS115 'COMMST100 'PHYS111 'MATH117))
                (list "Lebron" (list 'PHYS121 'PHYS122 'MATH145 'MATH147 'PD1))
                (list "James" (list 'CS145 'MATH147 'MATH145 'COMMST236))) "Owen" 'COMMST100)
              false)


;; c) add-course

;; (add-to-end lst item) consumes a list and an item and adds that item on to the end of the list.
;; Example:
(check-expect (add-to-end (list 1 2 3 4 5) 6)
              (list 1 2 3 4 5 6))
;; add-to-end: List Any -> List
(define (add-to-end lst item)
  (cond
    [(empty? lst) (list item)]
    [else (cons (first lst) (add-to-end (rest lst) item))]))

;; (already-taking-class student-choice course) consumes a list of courses and a symbol representing
;; a course and returns true if the student is already taking the course and false otherwise.
;; Example:
(check-expect (already-taking-class (list 'CS145 'MATH147 'MATH145 'COMMST236) 'CS145)
              true)
;; already-taking-class: List List -> Bool
(define (already-taking-class student-choice course)
  (cond
    [(empty? student-choice) false]
    [(symbol=? (first student-choice) course) true]
    [else (already-taking-class (rest student-choice) course)]))

;; (add-course selections quest-user course) consumes a DesiredCourses, a student's quest
;; username and a course code and adds the given code to the end of the list of courses.
;; Example:
(check-expect (add-course
               (list
                (list "Clay" (list 'CS135))
                (list "Steph" (list 'Math115 'CS115 'COMMST100 'PHYS111 'MATH117))
                (list "Lebron" (list 'PHYS121 'PHYS122 'MATH145 'MATH147 'PD1))
                (list "James" (list 'CS145 'MATH147 'MATH145 'COMMST236))) "Steph" 'CS116)
              (list
                (list "Clay" (list 'CS135))
                (list "Steph" (list 'Math115 'CS115 'COMMST100 'PHYS111 'MATH117 'CS116))
                (list "Lebron" (list 'PHYS121 'PHYS122 'MATH145 'MATH147 'PD1))
                (list "James" (list 'CS145 'MATH147 'MATH145 'COMMST236))))
;; add-course: (listof DesiredCourses) Str Sym -> (listof DesiredCourses)
(define (add-course selections quest-user course)
  (cond
    [(empty? selections) (list (list quest-user (list course)))]
    [(string=? (first (first selections)) quest-user)
     (cond
       [(already-taking-class (second (first selections)) course) selections]
       [else
        (cons (list (first (first selections)) (add-to-end (second (first selections)) course))
              (rest selections))])]
    [else (cons (first selections) (add-course (rest selections) quest-user course))]))
;; Tests:
(check-expect (add-course
               (list
                (list "Clay" (list 'CS135))
                (list "Steph" (list 'Math115 'CS115 'COMMST100 'PHYS111 'MATH117))
                (list "Lebron" (list 'PHYS121 'PHYS122 'MATH145 'MATH147 'PD1))
                (list "James" (list 'CS145 'MATH147 'MATH145 'COMMST236))) "James" 'CS145)
              (list
                (list "Clay" (list 'CS135))
                (list "Steph" (list 'Math115 'CS115 'COMMST100 'PHYS111 'MATH117))
                (list "Lebron" (list 'PHYS121 'PHYS122 'MATH145 'MATH147 'PD1))
                (list "James" (list 'CS145 'MATH147 'MATH145 'COMMST236))))
(check-expect (add-course empty "Owen" 'CS136L)
              (list (list "Owen" (list 'CS136L))))
     





;; d)
;; (create-classlist selections course) consumes a DesiredCourses and a course code and
;; produces a list of all the students that want to take the consumed course.
;; Example:
(check-expect (create-classlist
               (list
                (list "Clay" (list 'CS135))
                (list "Steph" (list 'Math115 'CS115 'COMMST100 'PHYS111 'MATH117))
                (list "Lebron" (list 'PHYS121 'PHYS122 'MATH145 'MATH147 'PD1))
                (list "James" (list 'CS135 'MATH147 'MATH145 'COMMST236))) 'CS135)
              (list "Clay" "James"))
;; create-classlist: DesiredCourses Symbol -> (listof Str)
(define (create-classlist selections course)
  (cond
    [(empty? selections) empty]
    [(user-taking-course? (second (first selections)) course)
     (cons (first (first selections)) (create-classlist (rest selections) course))];
    [else (create-classlist (rest selections) course)]))    
;; Tests:
(check-expect (create-classlist
               (list
                (list "Clay" (list 'CS135))
                (list "Steph" (list 'Math115 'CS115 'COMMST100 'PHYS111 'MATH117))
                (list "Lebron" (list 'PHYS121 'PHYS122 'MATH145 'MATH147 'PD1))
                (list "James" (list 'CS135 'MATH147 'MATH145 'COMMST236))) 'CS136)
              empty)

(check-expect (create-classlist selections 'MATH135) (list "mpines" "d32pines"))
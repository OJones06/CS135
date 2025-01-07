;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname pizza-party) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 05, Problem 3 "pizza-party.rkt"
;; ***************************************************


;; a)

;; Full data definitions and templates for StudentChoice, Section, Course

;; A StudentChoice is one of:
;; * empty
;; (list Str Sym Nat)
;; - where the Str represents the student's name
;; - the symbol is one of 'Hawaiian, 'meaty, or 'veggie
;; - the natural number represents the number of pizza slices

;; studentchoice-template: (listof StudentChoice) -> Any
(define (studentchoice-template sc)
  (... (first sc) ; student's name
       (second sc) ; desired pizza
       (third sc) ; number of slices
       ))



;; A Section is a:
;; Str Nat (listof StudentChoice)
;; - where the Str represents the instructor's name
;; - the natural number is the section number

;; section-template: Section -> Any
(define (section-template section)
     (... (first section) ; instructor
          (second section) ; section number
          (third section))) ; (listof StudentChoice)

;; A Course is one of:
;; * empty
;; (listof Section)

;; course-template: Course -> Any
(define (course-template course)
  (cond
    [(empty? course) ...]
    [else (... (first course) (course-template (rest course)))]
    ))


;; b)
;; (counter list pizza-type) consumes a (listof StudentChoice) and a type of pizza and produces
;; a Nat representing the number of apprearances of that pizza-type in the list.
;; Example:
(check-expect (counter (list (list "Owen" 'Hawaiian 5)
                             (list "Faith" 'meaty 3)
                             (list "Hyland" 'Hawaiian 3)
                             (list "Jack" 'veggie 1)
                             (list "Ryan" 'meaty 3)
                             (list "Ethan" 'meaty 2)) 'Hawaiian)
              2)
;; counter: (listof StudentChoice) Sym -> Nat
(define (counter list pizza-type)
  (cond
    [(empty? list) 0]
    [(symbol=? (second (first list)) pizza-type) (+ 1 (counter (rest list) pizza-type))]
    [else (+ 0 (counter (rest list) pizza-type))]))

;; (popular-pizza section) consumes a Section and produces the symbol of the most popular
;; type based on the number of students. There are no ties!
;; Example:
(check-expect (popular-pizza (list "Serry" 2 (list (list "Owen" 'Hawaiian 5)
                                                   (list "Faith" 'meaty 3)
                                                   (list "Hyland" 'Hawaiian 3)
                                                   (list "Jack" 'veggie 1)
                                                   (list "Ryan" 'meaty 3)
                                                   (list "Ethan" 'meaty 2))))
              'meaty)
;; popular-pizza: Section -> Sym
(define (popular-pizza section)
  (cond
    [(and (> (counter (third section) 'Hawaiian) (counter (third section) 'meaty))
          (> (counter (third section) 'Hawaiian) (counter (third section) 'veggie))) 'Hawaiian]
    [(and (> (counter (third section) 'meaty) (counter (third section) 'hawaiian))
          (> (counter (third section) 'meaty) (counter (third section) 'veggie))) 'meaty]
    [else 'veggie]))
;; Tests: 
(check-expect (popular-pizza (list "Patrick" 07 (list (list "JOE" 'Hawaiian 5)
                                                      (list "FRED" 'veggie 1)
                                                      (list "ALBERTO" 'veggie 1)
                                                      (list "DEZORAE" 'veggie 1)
                                                      (list "HUDSON" 'meaty 1)
                                                      (list "MARTY" 'Hawaiian 12))))
              'veggie)

(check-expect (popular-pizza (list "Reimer" 1000 (list (list "Keera" 'veggie 2)
                                                       (list "Mya" 'Hawaiian 2)
                                                       (list "Lydia" 'meaty 1)
                                                       (list "Penny" 'Hawaiian 3)
                                                       (list "Faith" 'Hawaiian 2)
                                                       (list "Fred" 'veggie 1))))
              'Hawaiian)








;; c)

;; (pizza-ranks pizza) consumes a pizza and produces a number that corresponds to that pizza's
;; ranking ('Hawaiian -> 'meaty -> 'veggie) (3 -> 2 -> 1).
;; Example:
(check-expect (pizza-ranks 'meaty) 2)
;; pizza-ranks: Sym -> Num
(define (pizza-ranks pizza)
  (cond
    [(symbol=? 'Hawaiian pizza) 3]
    [(symbol=? 'meaty pizza) 2]
    [(symbol=? 'veggie pizza) 1]
    [else 0])) 

;; (choices<= student1 student2) consumes two StudentChoices and produces true if the first
;; student choice should come before the second and false otherwise.
;; Example:
(check-expect (choices<= (list "Owen" 'meaty 4) (list "Faith" 'meaty 2)) false)
;; choices<=: StudentChoice StudentChoice -> Bool
(define (choices<= student1 student2)
  (cond
    [(> (pizza-ranks (second student1)) (pizza-ranks (second student2))) true]
    [(< (pizza-ranks (second student1)) (pizza-ranks (second student2))) false]
    [(string<=? (first student1) (first student2)) true]
    [else false]))

;; (insert student-choice slon) is a function that is altered from a function in M06.
;; it takes a StudentChoice and a list as input and places the StudentChoice in the
;; correct place in the list.
;; Example:
(check-expect (insert (list "Owen" 'meaty 4)
                      (list (list "Faith" 'meaty 3)
                            (list "Hyland" 'Hawaiian 3)
                            (list "Jack" 'veggie 1)
                            (list "Ryan" 'meaty 3)
                            (list "Ethan" 'meaty 2)))
              (list (list "Faith" 'meaty 3)
                    (list "Hyland" 'Hawaiian 3)
                    (list "Owen" 'meaty 4)
                    (list "Jack" 'veggie 1)
                    (list "Ryan" 'meaty 3)
                    (list "Ethan" 'meaty 2)))
;; insert: StudentChoice List -> List
(define (insert student-choice slon)
  (cond
    [(empty? slon) (cons student-choice empty)]
    [(choices<= student-choice (first slon)) (cons student-choice slon)]
    [else (cons (first slon) (insert student-choice (rest slon)))]))

;; (sorter section) consumes the choices of a given section and produces the same list with
;; sorted contents.
;; Example
(check-expect (sorter (list (list "Owen" 'Hawaiian 5)
                            (list "Faith" 'meaty 3)
                            (list "Hyland" 'Hawaiian 3)
                            (list "Jack" 'veggie 1)
                            (list "Ryan" 'meaty 3)
                            (list "Ethan" 'meaty 2)))
              (list (list "Hyland" 'Hawaiian 3)
                                    (list "Owen" 'Hawaiian 5)
                                    (list "Ethan" 'meaty 2)
                                    (list "Faith" 'meaty 3)
                                    (list "Ryan" 'meaty 3)
                                    (list "Jack" 'veggie 1)))
;; sorter: (listof StudentChoices) -> (listof StudentChoices)
(define (sorter section)
  (cond
    [(empty? section) empty]
    [else (insert (first section) (sorter (rest section)))]))

;; (sort-choices section) consumes a Section and produces the same section with sorted contents.
;; Example:
(check-expect (sort-choices (list "Serry" 2 (list (list "Owen" 'Hawaiian 5)
                                                  (list "Faith" 'meaty 3)
                                                  (list "Hyland" 'Hawaiian 3)
                                                  (list "Jack" 'veggie 1)
                                                  (list "Ryan" 'meaty 3)
                                                  (list "Ethan" 'meaty 2))))
              (list "Serry" 2 (list (list "Hyland" 'Hawaiian 3)
                                    (list "Owen" 'Hawaiian 5)
                                    (list "Ethan" 'meaty 2)
                                    (list "Faith" 'meaty 3)
                                    (list "Ryan" 'meaty 3)
                                    (list "Jack" 'veggie 1))))
;; sort-choices: Section -> Section
(define (sort-choices section)
  (list (first section) (second section) (sorter (third section))))

;; Tests:
(check-expect (sort-choices (list "Patrick" 07 (list (list "JOE" 'Hawaiian 5)
                                                     (list "FRED" 'veggie 1)
                                                     (list "ALBERTO" 'veggie 1)
                                                     (list "DEZORAE" 'veggie 1)
                                                     (list "HUDSON" 'meaty 1)
                                                     (list "MARTY" 'Hawaiian 12))))
              (list "Patrick" 07 (list (list "JOE" 'Hawaiian 5)
                                       (list "MARTY" 'Hawaiian 12)
                                       (list "HUDSON" 'meaty 1)
                                       (list "ALBERTO" 'veggie 1)
                                       (list "DEZORAE" 'veggie 1)
                                       (list "FRED" 'veggie 1))))

;; d)
;; (find-student section student-name) consumes a (listof StudentChoices) and a string
;; (student's name) and produces the type of pizza and number of slices of the student with
;; that name in the section.
;; Example:
(check-expect (find-student (list (list "JOE" 'Hawaiian 5)
                                  (list "MARTY" 'Hawaiian 12)
                                  (list "HUDSON" 'meaty 1)
                                  (list "ALBERTO" 'veggie 1)
                                  (list "DEZORAE" 'veggie 1)
                                  (list "FRED" 'veggie 1)) "FRED")
              (list 'veggie 1))
;; find-student: (listof StudentChoices) Str -> List
(define (find-student section student-name)
  (cond
    [(empty? section) 'error]
    [(string=? (first (first section)) student-name)
     (list (second (first section)) (third (first section)))]
    [else (find-student (rest section) student-name)]))

;; (pizza-lookup course section-num student-name) consumes a Course, a Num representing a section
;; and a Str representing a students name a produces a list containing the type of pizza and
;; the number of slices corresponding to the student.
;; Example:
(check-expect (pizza-lookup (list
                             (list "Patrick" 07 (list (list "JOE" 'Hawaiian 5)
                                                      (list "MARTY" 'Hawaiian 12)
                                                      (list "HUDSON" 'meaty 1)
                                                      (list "ALBERTO" 'veggie 1)
                                                      (list "DEZORAE" 'veggie 1)
                                                      (list "FRED" 'veggie 1)))
                             (list "Serry" 2 (list (list "Hyland" 'Hawaiian 3)
                                                   (list "Owen" 'Hawaiian 5)
                                                   (list "Ethan" 'meaty 2)
                                                   (list "Faith" 'meaty 3)
                                                   (list "Ryan" 'meaty 3)
                                                   (list "Jack" 'veggie 1)))) 07 "HUDSON")
              (list 'meaty 1))
;; pizza-lookup: Course Num Str -> List
(define (pizza-lookup course section-num student-name)
  (cond
    [(empty? course) 'error]
    [(= section-num (second (first course))) (find-student (third (first course)) student-name)]
    [else (pizza-lookup (rest course) section-num student-name)]
    ))


(check-expect (pizza-lookup (list
                             (list "Patrick" 07 (list (list "JOE" 'Hawaiian 5)
                                                      (list "MARTY" 'Hawaiian 12)
                                                      (list "HUDSON" 'meaty 1)
                                                      (list "ALBERTO" 'veggie 1)
                                                      (list "DEZORAE" 'veggie 1)
                                                      (list "FRED" 'veggie 1)))
                             (list "Serry" 2 (list (list "Hyland" 'Hawaiian 3)
                                                   (list "Owen" 'Hawaiian 5)
                                                   (list "Ethan" 'meaty 2)
                                                   (list "Faith" 'meaty 3)
                                                   (list "Ryan" 'meaty 3)
                                                   (list "Jack" 'veggie 1)))) 2 "Jack")
              (list 'veggie 1))

;; e)
;; Example:


;; The following three functions are almost identical but deal with different pizza types.
;; They count the number of slices of a certain pizza type in a given (listof StudentChoices)
;; Example:
(check-expect (count-haw (list (list "JOE" 'Hawaiian 5)
      (list "MARTY" 'Hawaiian 12)
      (list "HUDSON" 'meaty 1)
      (list "ALBERTO" 'veggie 1)
      (list "DEZORAE" 'veggie 1)
      (list "FRED" 'veggie 1))) 17)
;; count-haw: (listof StudentChoices) -> Num
(define (count-haw choices)
  (cond
    [(empty? choices) 0]
    [(symbol=? (second (first choices)) 'Hawaiian)
     (+ (third (first choices)) (count-haw (rest choices)))]
    [else (count-haw (rest choices))]))
;; count-meat: (listof StudentChoices) -> Num
(define (count-meat choices)
  (cond
    [(empty? choices) 0]
    [(symbol=? (second (first choices)) 'meaty)
     (+ (third (first choices)) (count-meat (rest choices)))]
    [else (count-meat (rest choices))]))
;; count-veg: (listof StudentChoices) -> Num
(define (count-veg choices)
  (cond
    [(empty? choices) 0]
    [(symbol=? (second (first choices)) 'veggie)
     (+ (third (first choices)) (count-veg (rest choices)))]
    [else (count-veg (rest choices))]))

;; (count-slices course) consumes a Course and produces a fixed length list of three natural number
;; that represents the number of each slice (list #'Haw -> #'meat -> #'veg)
;; Example:
(check-expect (count-slices (list
                             (list "Patrick" 07 (list (list "JOE" 'Hawaiian 5)
                                                      (list "MARTY" 'Hawaiian 12)
                                                      (list "HUDSON" 'meaty 1)
                                                      (list "ALBERTO" 'veggie 1)
                                                      (list "DEZORAE" 'veggie 1)
                                                      (list "FRED" 'veggie 1)))
                             (list "Serry" 2 (list (list "Hyland" 'Hawaiian 3)
                                                   (list "Owen" 'Hawaiian 5)
                                                   (list "Ethan" 'meaty 2)
                                                   (list "Faith" 'meaty 3)
                                                   (list "Ryan" 'meaty 3)
                                                   (list "Jack" 'veggie 1)))))
              (list 25 9 4))
;; count-slices: Course -> List
(define (count-slices course)
  (cond
    [(empty? course) (list 0 0 0)]
    [else
     (list (+ (count-haw (third (first course))) (first (count-slices (rest course))))
           (+ (count-meat (third (first course))) (second (count-slices (rest course))))
           (+ (count-veg (third (first course))) (third (count-slices (rest course)))))]))
;; Tests:
(check-expect (count-slices (list
                             (list "Patrick" 07 (list (list "JOE" 'Hawaiian 1)
                                                      (list "MARTY" 'Hawaiian 2)
                                                      (list "HUDSON" 'Hawaiian 3)
                                                      (list "ALBERTO" 'veggie 4)
                                                      (list "DEZORAE" 'veggie 5)
                                                      (list "FRED" 'veggie 6)))
                             (list "Serry" 2 (list (list "Hyland" 'Hawaiian 6)
                                                   (list "Owen" 'Hawaiian 5)
                                                   (list "Ethan" 'veggie 2)
                                                   (list "Faith" 'veggie 3)
                                                   (list "Ryan" 'veggie 3)
                                                   (list "Jack" 'veggie 1)))))
              (list 17 0 24)) 
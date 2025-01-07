;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus-a04) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 04, Bonus: "bonus-a04.rkt"
;; ***************************************************

;; BONUS

(define (same-position? answer guess)
  (cond
    [(= (length guess) 5)
     (char=? (first answer) (first guess))]
    [(= (length guess) 4)
     (char=? (first (rest answer)) (first guess))]
    [(= (length guess) 3)
     (char=? (first (rest (rest answer))) (first guess))]
    [(= (length guess) 2)
     (char=? (first (rest (rest (rest answer)))) (first guess))]
    [(= (length guess) 1)
     (char=? (first (rest (rest (rest (rest answer))))) (first guess))]))

(define (in-word? answer guess)
  (cond
    [(empty? answer) false]
    [(char=? (first guess) (first answer)) true]
    [else (in-word? (rest answer) guess)]))

(define (wordle-guess answer guess)
  (cond
    [(empty? guess) empty]
    [(same-position? answer guess) (cons 'green (wordle-guess answer (rest guess)))]
    [(in-word? answer guess) (cons 'yellow (wordle-guess answer (rest guess)))]
    [else (cons 'gray (wordle-guess answer (rest guess)))]))

(check-expect (wordle-guess
               (cons #\h (cons #\e (cons #\l (cons #\l (cons #\o empty)))))
               (cons #\s (cons #\t (cons #\o (cons #\l (cons #\e empty))))))
              (cons 'gray (cons 'gray (cons 'yellow (cons 'green (cons 'yellow empty))))))
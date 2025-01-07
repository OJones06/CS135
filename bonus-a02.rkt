;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus-a02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #f)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 02, Bonus: "bonus-a02.rkt"
;; ***************************************************
;; ***************************************************
;;   Credits: University of Waterloo: Alex Lopez-Ortiz
;;   Feb 23, 1998.
;;   https://cs.uwaterloo.ca/~alopez-o/math-faq/node73.html
;; ***************************************************
(define (date->day-of-week input)
  (cond
    [(= (day-of-week input) 2) 'Monday]
    [(= (day-of-week input) 3) 'Tuesday]
    [(= (day-of-week input) 4) 'Wednesday]
    [(= (day-of-week input) 5) 'Thursday]
    [(= (day-of-week input) 6) 'Friday]
    [(= (day-of-week input) 0) 'Saturday]
    [(= (day-of-week input) 1) 'Sunday]))

(define (is-leap-year year)
  (or (and (= (remainder year 4) 0)
           (not (= (remainder year 100) 0)))
      (= (remainder year 400) 0)))


(define (month-key input)
  (cond
    [(= input 1) 1]
    [(= input 2) 4]
    [(= input 3) 4]
    [(= input 4) 0]
    [(= input 5) 2]
    [(= input 6) 5]
    [(= input 7) 0]
    [(= input 8) 3]
    [(= input 9) 6]
    [(= input 10) 1]
    [(= input 11) 4]
    [(= input 12) 6]
    ))
(define (gregorian-adjustment input)
  (cond
    [(= 17 (quotient input 1000000)) 4]
    [(= 18 (quotient input 1000000)) 2]
    [(= 19 (quotient input 1000000)) 0]
    [(= 20 (quotient input 1000000)) 6]
    [else (gregorian-adjustment (- input 4000000))]))
  
(define (day-of-week input)
  (remainder
  (+
   ;; last two digits of year /4 discarding the fraction
   (quotient (remainder (quotient input 10000) 100) 4)
    
   ;; Day of the month
   (remainder input 100)
   ;; Add the months key value
   (month-key (quotient (remainder input 10000) 100))
   ;; Subtract one for January or February of a leap year
   (cond
     [(is-leap-year (quotient input 10000))
      (cond
        [(= 2 (quotient (remainder input 10000) 100)) -1]
        [(= 1 (quotient (remainder input 10000) 100)) -1]
        [else 0])]
     [else 0])
   ;; Gregorian-adjustment
   (gregorian-adjustment input)
   ;; Last two digits of the year
   (remainder (quotient input 10000) 100)
   ) 7))

;; Random check-expect statements for the date->day-of-week function
(check-expect (date->day-of-week 20230923) 'Saturday) ; September 23, 2023 -Saturday
(check-expect (date->day-of-week 19991231) 'Friday) ; December 31, 1999 - Friday
(check-expect (date->day-of-week 20240515) 'Wednesday) ; May 15, 2024 - Wednesday
(check-expect (date->day-of-week 19850720) 'Saturday) ; July 20, 1985 - Saturday
(check-expect (date->day-of-week 20001225) 'Monday) ; December 25, 2000 - Monday
(check-expect (date->day-of-week 19760617) 'Thursday) ; June 17, 1976 - Thursday
(check-expect (date->day-of-week 20250202) 'Sunday) ; February 2, 2025 - Sunday
(check-expect (date->day-of-week 19450704) 'Wednesday) ; July 4, 1945 - Wednesday
(check-expect (date->day-of-week 20211111) 'Thursday) ; November 11, 2021 - Thursday
(check-expect (date->day-of-week 19630301) 'Friday) ; March 1, 1963 - Friday
(check-expect (date->day-of-week 20160620) 'Monday) ; June 20, 2016 - Monday
(check-expect (date->day-of-week 19501231) 'Sunday) ; December 31, 1950 - Sunday
(check-expect (date->day-of-week 20090214) 'Saturday) ; February 14, 2009 - Saturday
(check-expect (date->day-of-week 19880430) 'Saturday) ; April 30, 1988 - Saturday
(check-expect (date->day-of-week 20301123) 'Saturday) ; November 23, 2030 - Saturday
(check-expect (date->day-of-week 19010701) 'Monday) ; July 1, 1901 - Monday
(check-expect (date->day-of-week 20000808) 'Tuesday) ; August 8, 2000 - Tuesday
(check-expect (date->day-of-week 20221225) 'Sunday) ; December 25, 2022 - Sunday
(check-expect (date->day-of-week 19781203) 'Sunday) ; December 3, 1978 - Sunday
(check-expect (date->day-of-week 20450618) 'Sunday) ; June 18, 2045 - Sunday
(check-expect (date->day-of-week 20240218) 'Sunday) ; February 18, 2024 - Sunday
(check-expect (date->day-of-week 20440102) 'Saturday) ; January 2, 2044 - Saturday
(check-expect (date->day-of-week 17740502) 'Monday) ; May 2, 1774 - Monday
(check-expect (date->day-of-week 18361225) 'Sunday) ; December 25, 1836 - Sunday
(check-expect (date->day-of-week 38741009) 'Friday) ; October 9, 3874 - Friday
(check-expect (date->day-of-week 19980223) 'Monday) ; February 23, 1998 - Monday
(check-expect (date->day-of-week 20000229) 'Tuesday) ; February 29, 2000 - Tuesday
(check-expect (date->day-of-week 19000228) 'Wednesday) ; February 28, 1900 - Wednesday

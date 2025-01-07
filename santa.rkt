;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname santa) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 06, Problem 3 "santa.rkt"
;; ***************************************************

;; Q3.
;; a)

;; A Name is a Str

;; A Desc is a Str
;; Note: This could be a description of an Action or a Wish (part c).

;; A NiceScore n is an Int
;; Requires: -100 <= n <= 100 and n not= 0
;(define-struct wish (score gift))
(define-struct action (niceness desc))
;; An Action is a (make-action NiceScore Desc)

;; An ActionList is a (listof (list Name (listof Action)))
;; Requires: The list is sorted alphabetically by child name.
;;           Each list of Actions is non-empty.

;; Note: the order of Actions for the same child is arbitrary.


;; (extreme-actions name loACT) consumes a child's name and an ActionList and produces either:
;; - empty if the child's name does not appear in the ActionList
;; - a list of two strings where the first is the description of the child's action with the lowest
;; niceness score and the second is the description of the child's action with the highest score.
;; Example:
 (check-expect (extreme-actions "Charlotte"
                               (list
                                (list "Barry"
                                      (list
                                       (make-action 10 "Won the running event")
                                       (make-action -1 "Stayed up too late on a schoolnight")
                                       (make-action 6 "Helped some friends get to school on time")))
                                (list "Casandra"
                                      (list
                                       (make-action -10 "failed a test")
                                       (make-action 20 "held a door open")
                                       (make-action 0 "slept")))))
              empty)
;; extreme-actions: Str (listof Act) -> (listof Str)
(define (extreme-actions name loACT)
  (cond
    [(empty? loACT) empty]
    [(string=? name (first (first loACT))) (find-min/max
                                            (first (rest (first loACT)))
                                            (make-action 0 "") (make-action 0 ""))]
    [else (extreme-actions name (rest loACT))]))
;; Tests:
(check-expect (extreme-actions
               "Charlotte"
               (list (list "Barry" (list
                                    (make-action 10 "Won the running event")
                                    (make-action -1 "Stayed up too late on a schoolnight")
                                    (make-action 6 "Helped some friends get to school on time")))
                     (list "Charlotte" (list
                                        (make-action -10 "failed a test")
                                        (make-action 20 "held a door open")
                                        (make-action 0 "slept")))))
              (list "failed a test" "held a door open"))

;; (find-min/max actions min max) consumes a (listof Action), Action Action and produces a
;; list containing the descriptions of the lowest and greatest action.
;; Example:
(check-expect (find-min/max (list
                             (make-action 10 "Won the running event")
                             (make-action -1 "Stayed up too late on a schoolnight")
                             (make-action 6 "Helped some friends get to school on time"))
                            (make-action 0 "") (make-action 0 ""))
              (list "Stayed up too late on a schoolnight" "Won the running event"))
;; find-min/max: (listof Action) Action Action -> (List Str Str)
(define (find-min/max actions min max)
  (cond
    [(empty? actions) (list (action-desc min) (action-desc max))]
    [(> (action-niceness (first actions)) (action-niceness max))
     (find-min/max (rest actions) min (first actions))]
    [(< (action-niceness (first actions)) (action-niceness min))
     (find-min/max (rest actions) (first actions) max)]
    [else (find-min/max (rest actions) min max)]))


;; b)
;; An ActionUpdate is a (listof (list Name Action))
;; Requires: The list is sorted alphabetically by child name.
;;           A name will only appear once in the list.

;; (merge-actions action-list action-update) consumes a ActionList and an ActionUpdate and produces
;; a new ActionList with any new children and action included. Placing the new Action from
;; ActionUpdate at the front.
;; Example:
(check-expect (merge-actions
               (list
                (list "Owen"
                      (list (make-action 1 "did this"))))
               (list
                (list "Owen" (make-action 1 "did that"))))
              (list
               (list "Owen" (list (make-action 1 "did that")
                                  (make-action 1 "did this")))))
;; merge-actions: ActionList ActionUpdate -> ActionList
(define (merge-actions action-list action-update)
  (cond
    [(empty? action-list) action-update]
    [(empty? action-update) action-list]
    [(string=? (first (first action-list)) (first (first action-update)))
     (cons (list (first (first action-list))
                 (cons (second (first action-update))
                       (second (first action-list))))
           (merge-actions (rest action-list) (rest action-update)))]
    [else (cons (first action-update) (merge-actions action-list (rest action-update)))]))
;; Tests:
(check-expect (merge-actions
               (list
                (list "Justin"
                      (list (make-action 5 "did a great thing")
                            (make-action 1 "did a thing")))
                (list "Marco"
                      (list (make-action 10 "did an amazing thing")
                            (make-action -10 "did an awful thing"))))
               (list (list "Justin" (make-action 10 "did a great thing"))
                     (list "Marco" (make-action 1 "did a mediocre thing"))))
              (list
               (list "Justin"
                     (list (make-action 10 "did a great thing")
                           (make-action 5 "did a great thing")
                           (make-action 1 "did a thing")))
               (list "Marco"
                     (list (make-action 1 "did a mediocre thing")
                           (make-action 10 "did an amazing thing")
                           (make-action -10 "did an awful thing")))))
(check-expect (merge-actions
               (list
                (list "Joe"
                      (list (make-action 100 "did the best thing")
                            (make-action 55 "pet a dog")
                            (make-action -15 "punched the dog")))
                (list "Bo"
                      (list (make-action 99 "touchdown")
                            (make-action 14 "first down")
                            (make-action -12 "fumble"))))
               (list (list "Joe" (make-action 50 "awesome thing"))
                     (list "Bo" (make-action -200 "pick six"))))
              (list
               (list "Joe"
                     (list (make-action 50 "awesome thing")
                           (make-action 100 "did the best thing")
                           (make-action 55 "pet a dog")
                           (make-action -15 "punched the dog")))
               (list "Bo"
                     (list (make-action -200 "pick six")
                           (make-action 99 "touchdown")
                           (make-action 14 "first down")
                           (make-action -12 "fumble")))))


;; c)

(define-struct wish (score gift))
;; A Wish is a (make-wish NiceScore Desc)
;; Requires: score is further restricted to be > 0

;; A WishList is a (listof Wish)
;; Requires: Wishes are sorted in non-decreasing order by score.

;; A ChildrenList is a (listof (list Name Wishlist))
;; Requires: The list is sorted alphabetically by child name.



(define super-actlst  
  (list (list "Barry" (list (make-action 10 "Won the running event")
                            (make-action -1 "Stayed up too late on a schoolnight")
                            (make-action 6 "Helped some friends get to school on time")))
        (list "Bruce" (list (make-action -4 "Pretended to be a bat and scared children in the park")))
        (list "Clark" (list (make-action -5 "Skipped his chores on the farm")
                            (make-action 5 "Saved Earth from ...")))
        (list "Hal" (list (make-action 3 "Shared his toys with the other children")))
        (list "Harley" (list (make-action -10 "Joined a gang")))
        (list "Zatanna" (list (make-action 15 "Show friends magic tricks")
                            (make-action 3 "Studied for their exam")
                            (make-action -7 "Made a mess when magic went wrong")
                            (make-action -4 "Procrastinated on CS 135 assignment")))))

(define super-chldlst
  (list (list "Barry" (list (make-wish 3 "New boots") (make-wish 4 "Red suit") (make-wish 20 "Fancy hat")))
        (list "Clark" (list (make-wish 1 "Red cape")))
        (list "Zatanna" (list (make-wish 3 "Tophat") (make-wish 5 "New wand")))))


;; Testing for choose-gifts in part c)
(check-expect (choose-gifts -1 empty) (list "coal"))
(check-expect (choose-gifts -1 (list (make-wish 1 "Red cape"))) (list "coal"))
(check-expect (choose-gifts 0 empty) (list "socks"))
(check-expect (choose-gifts 0 (list (make-wish 1 "Red cape"))) (list "socks"))
(check-expect (choose-gifts 42 empty) (list "socks"))
(check-expect (choose-gifts 2 (list (make-wish 3 "Tophat") (make-wish 5 "New wand"))) (list "socks"))
(check-expect (choose-gifts 4 (list (make-wish 3 "Tophat") (make-wish 5 "New wand"))) (list "Tophat"))
(check-expect (choose-gifts 10 (list (make-wish 3 "Tophat") (make-wish 5 "New wand"))) (list "New wand" "Tophat"))


;; (choose-gifts niceness lowish) consumes an Int representing the child's niceness score and a
;; (listof Wish) and produces a (listof Desc) which represents the gift(s) that the child recieves.
;; Example:
 (check-expect (choose-gifts 10 (list
                                (make-wish 15 "xbox")
                                (make-wish 12 "jersey")
                                (make-wish 9 "hat")
                                (make-wish 2 "water bottle")))
              (list "water bottle" "hat"))
;; choose-gifts: Int WishList -> (listof Str)
(define (choose-gifts niceness lowish)
  (cond
    [(> 0 niceness) (cons "coal" empty)]
    [(empty? lowish) (cons "socks" empty)]
    [else (accumulate-gifts niceness lowish empty)]))
;; Tests:
(check-expect (choose-gifts 7 (list
                               (make-wish 50 "car")
                               (make-wish 25 "iphone")
                               (make-wish 9 "controller")))
              (list "socks"))
(check-expect (choose-gifts 50 empty) (list "socks"))
(check-expect (choose-gifts 50 (list
                                (make-wish 50 "car")
                                (make-wish 25 "iphone")
                                (make-wish 9 "controller")))
              (list "controller" "iphone" "car"))
(check-expect (choose-gifts -31 (list
                                 (make-wish 100 "gun")
                                 (make-wish 25 "candy")
                                 (make-wish 0 "socks")))
              (list "coal"))
(check-expect (choose-gifts 0 (list
                               (make-wish 55 "airpods")
                               (make-wish 2 "plant")
                               (make-wish 1 "clock")))
              (list "socks"))

;; (accumulate-gifts niceness lowish gifts) consumes a Int, WishList and an empty list accumulator
;; and produces a list of the gifts in the wishlist that the child deserves based on the niceness
;; score with the best gift at the end.
;; Example:
(check-expect (accumulate-gifts 10 (list
                               (make-wish 15 "controller")
                               (make-wish 7 "plant")
                               (make-wish 3 "rubber ducky")
                               (make-wish 1 "sunscreen")) empty)
              (list "sunscreen" "rubber ducky" "plant"))
;; accumulate-gifts: Int WishList empty -> (listof Str)  
(define (accumulate-gifts niceness lowish gifts)
  (cond
    [(and (empty? lowish) (empty? gifts)) (list "socks")]
    [(empty? lowish) gifts]
    ;[(and (empty? (rest lowish)) (< niceness (wish-score (first lowish))))
     ;(cons "socks" empty)]
    [(>= niceness (wish-score (first lowish)))
     (accumulate-gifts niceness (rest lowish) (cons (wish-gift (first lowish)) gifts))]
    [else (accumulate-gifts niceness (rest lowish) gifts)]))

;; (assign-gifts loACT loCHILD) consumes a ActionList and a ChildrenList and produces a GiftList
;; A child's niceness score is the sum of all actions and determies the gifts the child recieves.
;; Example:
(check-expect (assign-gifts
               (list 
                (list "Barry" (list
                               (make-action 10 "Won the running event")
                               (make-action -1 "Stayed up too late on a schoolnight")
                               (make-action 6 "Helped some friends get to school on time")))
                (list "Dom" (list
                             (make-action -10 "terrible thing")
                             (make-action -15 "worse thing")))
                (list "Elias" empty)
                (list "Owen" (list
                              (make-action 10 "good thing")
                              (make-action -10 "just as bad thing")))
                (list "Trey" (list
                              (make-action 10 "Won the running event")
                              (make-action -1 "Stayed up too late on a schoolnight")
                              (make-action 6 "Helped some friends get to school on time"))))
               (list 
                (list "Barry" (list
                               (make-wish 15 "controller")
                               (make-wish 7 "plant")
                               (make-wish 3 "rubber ducky")
                               (make-wish 1 "sunscreen")))
                (list "Dom" (list
                             (make-wish 15 "controller")
                             (make-wish 7 "plant")
                             (make-wish 3 "rubber ducky")
                             (make-wish 1 "sunscreen")))
                (list "Elias" (list
                               (make-wish 15 "controller")
                               (make-wish 7 "plant")
                               (make-wish 3 "rubber ducky")
                               (make-wish 1 "sunscreen")))
                (list "Owen" (list
                               (make-wish 15 "controller")
                               (make-wish 7 "plant")
                               (make-wish 3 "rubber ducky")
                               (make-wish 1 "sunscreen")))))
              (list 
               (list "Barry" (list "sunscreen" "rubber ducky" "plant" "controller"))
               (list "Dom" (list "coal"))
               (list "Elias" (list "socks"))
               (list "Owen" (list "socks"))
               (list "Trey" (list "socks"))))
;; assign-gifts: ActionList ChildrenList -> (listof (list Str (listof Str)))
(check-expect (assign-gifts (list
                             (list "Barry" (list
                                            (make-action 10 "did a good thing")))
                             (list "Caden" (list
                                           (make-action -10 "bad thing"))))
                            (list
                             (list "Cedric" (list
                                             (make-wish 10 "wants a toy")))))
              (list
               (list "Barry" (list "socks"))
               (list "Caden" (list "coal"))))
(define (assign-gifts loACT loCHILD)
  (cond
    [(empty? loACT) empty]
    [(empty? loCHILD) (give-rest-socks loACT)]
    [(and (string<? (first (first loACT)) (first (first loCHILD)))
          (>= (sum-actions (second (first loACT))) 0))
     (cons (list (first (first loACT)) (list "socks")) (assign-gifts (rest loACT) loCHILD))]
    [(and (string<? (first (first loACT)) (first (first loCHILD)))
          (< (sum-actions (second (first loACT))) 0))
     (cons (list (first (first loACT)) (list "coal")) (assign-gifts (rest loACT) loCHILD))]
    [else (cons (list (first (first loACT))
                      (choose-gifts (sum-actions (second (first loACT))) (second (first loCHILD))))
                (assign-gifts (rest loACT) (rest loCHILD)))]))

(define (give-rest-socks loCHILD)
  (cond
    [(empty? loCHILD) empty]
    [else (cons (list (first (first loCHILD)) (list "socks")) (give-rest-socks (rest loCHILD)))]))
         
;; (sum-actions loACT) consumes a child's actions and produces the sum of the child's actions.
;; Example:
 (check-expect (sum-actions (list
                               (make-action 10 "Won the running event")
                               (make-action -1 "Stayed up too late on a schoolnight")
                               (make-action 6 "Helped some friends get to school on time")))
              15) 
;; sum-actions: ActionList -> Int
(define (sum-actions loACT)
  (cond
    [(empty? loACT) 0]
    [else (+ (action-niceness (first loACT)) (sum-actions (rest loACT)))]))
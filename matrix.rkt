;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname matrix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;;   Owen Jones (21116342)
;;   CS 135 Fall 2024
;;   Assignment 06, Problem 2 "matrix.rkt"
;; ***************************************************

;; Q2:
;; a) Write a data definition for a Matrix.

;; A Matrix is a (listof (listof Num))
;; Where the length of each (list Num) is constant.
;; - The number of (list Num)s is the number of rows
;; - The length of (list Num) is the number of columns

;; b)
;; (matrix-item matrix row column) consumes a Matrix, a row nummber and a column number and
;; produces the item at that row and column position.
;; Example:
(check-expect (matrix-item (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) 0 2) 3)
;; matrix-item: Matrix Nat Nat -> Num
;; Requires:
;; row < #of (list Num)s
;; column < length of (list Num) 
(define (matrix-item matrix row column)
  (cond
    [(> row 0) (matrix-item (rest matrix) (- row 1) column)]
    [else (matrix-column (first matrix) column)]))
;; (matrix-column column index) consumes a column and an index and produces the value at that index.
;; Example:
(check-expect (matrix-column (list 1 2 3) 2) 3)
;; matrix-column: (listof Num) Nat -> Num
(define (matrix-column column index)
  (cond
    [(= 0 index) (first column)]
    [else (matrix-column (rest column) (- index 1))]))
;; Tests:
(check-expect (matrix-item (list (list 9 8 10) (list 2 -5 2) (list -99 -100 -101))
                           0 2) 10)
(check-expect (matrix-item (list (list 2 4 6 8 9) (list 1 3 5 7 11)) 1 3) 7)

;; c)
;; (matrix-col matrix column) consumes a Matrix, and a column number and produces that column
;; of the matrix.
;; Example:
(check-expect (matrix-col (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) 1) (list 2 5 8))
;; matrix-col: Matrix Nat -> (listof Num)
(define (matrix-col matrix column)
  (cond
    [(empty? matrix) empty]
    [(= 0 column) (first-of-all matrix)]
    [else (matrix-col (matrix-1col matrix) (- column 1))]))

;; (matrix-1col matrix) consumes a Matrix and produces the same Matrix with the first column removed.
;; Example:
(check-expect (matrix-1col (list (list 1 2 3) (list 3 4 5)))
              (list (list 2 3) (list 4 5)))
;; matrix-1: Matrix -> Matrix
(define (matrix-1col matrix)
  (cond
    [(empty? matrix) empty]
    [else (cons (rest (first matrix)) (matrix-1col (rest matrix)))]))

;; (first-of-all matrix) consumes a Matrix and produces the first column.
;; Example:
(check-expect (first-of-all (list (list 2 3) (list 4 5)))
              (list 2 4))
;; first-of-all: Matrix -> (listof Num)
(define (first-of-all matrix)
  (cond
    [(empty? matrix) empty]
    [else (cons (first (first matrix)) (first-of-all (rest matrix)))]))
;; Tests:
(check-expect (matrix-col (list (list 9 8 10) (list 2 -5 2) (list -99 -100 -101))
                          2) (list 10 2 -101))
(check-expect (matrix-col (list (list 2 4 6 8 9) (list 1 3 5 7 11)) 3) (list 8 7))

;; d)
;; (matrix-transpose matrix) consumes a Matrix and produces the transpose of the one consumed.
;; The transpose of a matrix A is another matrix where each row of A becomes a column of the new
;; matrix, essentially rotating the matrix.
;; Example:
(check-expect (matrix-transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
              (list (list 1 4 7) (list 2 5 8) (list 3 6 9)))
;; matrix-transpose: Matrix -> Matrix
(define (matrix-transpose matrix)
  (cond
    [(empty? matrix) empty]
    [(empty? (first matrix)) empty]
    [else (cons (first-column matrix) (matrix-transpose (remove-column matrix)))]))
;; Tests:
(check-expect (matrix-transpose (list (list 'a 'b 'c) (list true false true) (list "e" "f" "g")))
              (list (list 'a true "e") (list 'b false "f") (list 'c true "g")))
(check-expect (matrix-transpose (list (list 2 4 6 8 9) (list 1 3 5 7 11)))
              (list (list 2 1) (list 4 3) (list 6 5) (list 8 7) (list 9 11)))

;; (first-column matrix) consumes a Matrix and produces the first column in list form which represents
;; the new matrix's row.
(check-expect (first-column (list (list 1 2) (list 3 4))) (list 1 3))
(define (first-column matrix)
  (cond
    [(empty? matrix) empty]
    [else (cons (first (first matrix)) (first-column (rest matrix)))]))

;; (remove-column matrix) consumes a Matrix and produces the same matrix with the first column
;; removed.
;; Example:
(check-expect (remove-column (list (list 1 2) (list 3 4))) (list (list 2) (list 4)))
;; remove-column: Matrix -> Matrix
(define (remove-column matrix)
  (cond
    [(empty? matrix) empty]
    [(empty? (first matrix)) empty]
    [else (cons (rest (first matrix)) (remove-column (rest matrix)))]))

;; e)
;; (matrix-multiply matrix1 matrix2) consumes two matrices and produces the result of
;; multiplying them.
;; Example:
(check-expect (matrix-multiply (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8)))
              (list (list 19 22) (list 43 50)))
;; matrix-multiply: Matrix Matrix -> Matrix
(define (matrix-multiply matrix1 matrix2)
  (cond
    [(or (empty? matrix1) (empty? matrix2)) empty]
    [else (cons (matrix-dot-product (first matrix1) matrix2)
                (matrix-multiply (rest matrix1) matrix2))]))

;; (matrix-dot-product row1 matrix2) computes the row-wise dot products of row1 with
;; each column of matrix2.
;; Example:
(check-expect (matrix-dot-product (list 1 2) (list (list 5 6) (list 7 8)))
              (list 19 22))
;; matrix-dot-product: (listof Num) Matrix -> (listof Num)
(define (matrix-dot-product row1 matrix2)
  (cond
    [(empty? (first matrix2)) empty]
    [else (cons (new-row row1 (first-column matrix2))
                (matrix-dot-product row1 (remove-column matrix2)))]))

;; (new-row matrix1-row matrix2-column) consumes a row and a column and returns the dot
;; product of these lists.
;; Example:
(check-expect (new-row (list 1 2) (list 5 6)) 17)
;; new-row: (listof Num) (listof Num) -> Num
(define (new-row matrix1-row matrix2-column)
  (cond
    [(empty? matrix1-row) 0]
    [else (+ (* (first matrix1-row) (first matrix2-column))
             (new-row (rest matrix1-row) (rest matrix2-column)))]))

;; Tests:
(check-expect (matrix-multiply (list (list 5 6 7) (list 1 5 10) (list 2 4 6))
                               (list (list -5 -1 -2) (list 3 -2 0) (list 9 8 7)))
              (list (list 56 39 39) (list 100 69 68) (list 56 38 38)))
(check-expect (matrix-multiply (list (list 1 2) (list 3 4) (list 5 6))
                               (list (list 5 4) (list 3 0)))
              (list (list 11 4) (list 27 12) (list 43 20)))
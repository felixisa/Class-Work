;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Felix-Isabella-Mutation-Quiz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
#|
FOR INSTRUCTOR USE ONLY

Total:  100/100
Grade:  A+

Great job!

Q1

A lfunc is an interface for linera functions: y = mx + b. To create a
an lfunc you need, m, the slope and you need, b, the y-intercept. The
following operations are valid on a lfunc:

get-slope: number
get-yintercept: number
set-slope!: number --> (void) (a mutator to change the slope)
set-yintercept!: number --> (void) (a mutator to change the y-intercept)
value-at: number --> number (a function for the value of y)
add-f: lfunc --> lfunc (a function to make a new lfunc for this lfunct + the given lfunct)
       Example
             f: y = 3x + 1
             g: y = 5x + 3
         f + g: y = 8x + 4

Design and implement the interface lfunc.
|#

; An lfunc is an interface for linear functions: y = mx + b
; 1. 'get-slope: number
; 2. 'get-yintercept: number
; 3. 'set-slope!: number -> (void)
; 4. 'set-yintercept!: number -> (void)
; 5. 'value-at: number -> number
; 6. 'add-f: lfunc -> lfunc

(define (make-lfunc slope y-int)
  (local [; m is a number used to maintain the slope of the lfunc
          (define m slope)
          ; b is a number used to maintain the y-intercept of the lfunc
          (define b y-int)

          ; set-slope!: number -> void
          ; Purpose: To change the slope of the lfunc to the given value
          ; Effects: m will be set to a new given value
          (define (set-slope! new-m)
            ; INVENTORY
            ; m is the slope
            ; b is the y-intercept
            ; (set! m ...) mutates the value of m
            ; (set! b ...) mutates the value of b
            
            (local [(define u (set! m new-m))]
              (void)))

          ; set-yintercept!: number -> void
          ; Purpose: To change the y intercept of the lfunc
          ; Effects: b will be set to a new given value
          (define (set-yintercept! new-b)
            ; INVENTORY
            ; m is the slope
            ; b is the y-intercept
            ; (set! m ...) mutates the value of m
            ; (set! b ...) mutates the value of b
            
            (local [(define u (set! b new-b))]
              (void)))

          ; value-at: number -> number
          ; Purpose: To find the value of y at the given x value
          (define (value-at x)
            ; INVENTORY
            ; m is the slope
            ; b is the y-intercept
            (+ (* m x) b))

          ; lfunc-manager: an lfunc object
          ; Purpose: To manage inquiry of slope or y-intercept,
          ;          changes to slope or y-intercept, inquiry
          ;          of y value at given x, and addition of lfuncs
          (define (lfunc-manager msg)
            (cond [(eq? msg 'get-slope) m]
                  [(eq? msg 'get-yintercept) b]
                  [(eq? msg 'set-slope) set-slope!]
                  [(eq? msg 'set-yintercept) set-yintercept!]
                  [(eq? msg 'value-at) value-at]
                  [else (error 'lfunc-manager (format "Unknown Service Requested: ~s" msg))]))]
    lfunc-manager))

; slope: lfunc -> number
; Purpose: Wrap function to inquire the slope of an lfunc
(define (slope lfunc)
  (lfunc 'get-slope))

(check-expect (slope f) 4)
(check-expect (slope g) -3)

; y-int: lfunc -> number
; Purpose: Wrap function to inquire the y-intercept of an lfunc
(define (y-int lfunc)
  (lfunc 'get-yintercept))

(check-expect (y-int f) 5)
(check-expect (y-int g) 9) 

; change-slope: lfunc number -> void
; Purpose: Wrap function to alter the slope of an lfunc
(define (change-slope lfunc n)
  ((lfunc 'set-slope) n))

(check-expect (begin
                (change-slope f 8)
                (slope f))
              8)

(check-expect (begin
                (change-slope f 4)
                (slope f))
              4)

; change-yint: lfunc number -> void
; Purpose: Wrap function to alter the y-intercept of an lfunc
(define (change-yint lfunc n)
  ((lfunc 'set-yintercept) n))

(check-expect (begin
                (change-yint g 0)
                (y-int g))
              0)

(check-expect (begin
                (change-yint g 9)
                (y-int g))
              9)

; y-value-at: lfunc number -> number
; Purpose: Wrap function to find the value of y at the given x
(define (y-value-at lfunc x)
  ((lfunc 'value-at) x))

(check-expect (y-value-at f 3) 17)
(check-expect (y-value-at g 0) 9)

; add-lfuncs: lfunc -> lfunc
; Purpose: To create a new lfunc that is the result of adding the
;          two given lfuncs
(define (add-lfuncs lfunc1 lfunc2) 
  (make-lfunc (+ (slope lfunc1) (slope lfunc2))
              (+ (y-int lfunc1) (y-int lfunc2))))

(check-expect (slope (add-lfuncs f g)) 1)
(check-expect (y-int (add-lfuncs f g)) 14)

; y = 4x + 5
(define f (make-lfunc 4 5))

; y = -3x + 9
(define g (make-lfunc -3 9))
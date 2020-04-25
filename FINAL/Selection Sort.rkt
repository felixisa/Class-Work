;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Selection Sort|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "while.rkt")

;; SELECTION SORT

; selection-sort!: (vectorof number) -> (void)
; Purpose: To sort the array in non-decreasing order.
; Effect: The elements of the array are rearranged in place.
(define (selection-sort! V)
  (local [; swap: natnum natnum -> (void)
          ; Purpose: To swap V[i] and V[j]
          ; Effect: Modify V[i] to contain the value of V[j] and vice versa
          (define (swap! i j)
            (local [(define temp (vector-ref V i))]
              (begin
                (vector-set! V i (vector-ref V j))
                (vector-set! V j temp)))) 
          ; select: natnum natnum -> natnum
          ; Purpose: To return the index of the smallest element in the vector
          (define (select low high)
            (local [; natnum
                    ; Purpose: the low index of the interval beign processed
                    (define i (void))
                    ; natnum
                    ; Purpose: the high index of the interval being processed
                    (define j (void))
                    ; natnum
                    ; Purpose: the lowest element in the interval being processed 
                    (define smallest (void))]
              (begin
                (set! i low)
                (set! j high)
                (set! smallest low)
                (while (< i j)
                       (if (<= (vector-ref V i) (vector-ref V smallest))
                           (set! smallest i)
                           (set! smallest smallest))
                       (set! i (add1 i))) ; close while 
                smallest)))
          ; natnum
          ; Purpose: the low element of the interval being processed 
          (define low (void))
          ; natnum
          ; Purpose: the high element of the interval being processed 
          (define high (void))]
    (begin
      (set! low 0)
      (set! high (sub1 (vector-length V)))
      (while (< low high)
             (if (> (vector-ref V low)
                    (vector-ref V (select (add1 low) high)))
                 (swap! low (select (add1 low) high))
                 (set! low low))
             (set! low (add1 low))))))

(define v (vector 4 2 9 10 7))

(check-expect (begin
                (selection-sort! v)
                v)
             (vector 2 4 9 7 10)) 

;; VECTORS
(define V500 (build-vector 500 (lambda (i) (random 1000000))))
(define V1000 (build-vector 1000 (lambda (i) (random 1000000))))
(define V1500 (build-vector 1500 (lambda (i) (random 1000000))))
(define V2000 (build-vector 2000 (lambda (i) (random 1000000))))
(define V2500 (build-vector 2500 (lambda (i) (random 1000000))))
(define V3000 (build-vector 3000 (lambda (i) (random 1000000))))
(define V3500 (build-vector 3500 (lambda (i) (random 1000000))))
(define V4000 (build-vector 4000 (lambda (i) (random 1000000))))
(define V4500 (build-vector 4500 (lambda (i) (random 1000000))))
(define V5000 (build-vector 5000 (lambda (i) (random 1000000))))
(define V5500 (build-vector 5500 (lambda (i) (random 1000000))))
(define V6000 (build-vector 6000 (lambda (i) (random 1000000))))
(define V6500 (build-vector 6500 (lambda (i) (random 1000000))))
(define V7000 (build-vector 7000 (lambda (i) (random 1000000))))
(define V7500 (build-vector 7500 (lambda (i) (random 1000000))))
(define V8000 (build-vector 8000 (lambda (i) (random 1000000))))
(define V8500 (build-vector 8500 (lambda (i) (random 1000000))))
(define V9000 (build-vector 9000 (lambda (i) (random 1000000))))
(define V9500 (build-vector 9500 (lambda (i) (random 1000000))))
(define V10000 (build-vector 10000 (lambda (i) (random 1000000))))
(define V10500 (build-vector 10500 (lambda (i) (random 1000000))))
(define V11000 (build-vector 11000 (lambda (i) (random 1000000))))
(define V11500 (build-vector 11500 (lambda (i) (random 1000000))))
(define V12000 (build-vector 12000 (lambda (i) (random 1000000))))
(define V12500 (build-vector 12500 (lambda (i) (random 1000000))))
(define V13000 (build-vector 13000 (lambda (i) (random 1000000))))
(define V13500 (build-vector 13500 (lambda (i) (random 1000000))))
(define V14000 (build-vector 14000 (lambda (i) (random 1000000))))
(define V14500 (build-vector 14500 (lambda (i) (random 1000000))))
(define V15000 (build-vector 15000 (lambda (i) (random 1000000))))

;; TIMING 
(begin
  (display "Selection Sort 500")
  (newline)
  (time (selection-sort! V500))
  (newline)
  (display "Selection Sort 1000")
  (newline)
  (time (selection-sort! V1000))
  (newline)
  (display "Selection Sort 1500")
  (newline)
  (time (selection-sort! V1500))
  (newline)
  (display "Selection Sort 2000")
  (newline)
  (time (selection-sort! V2000))
  (newline)
  (display "Selection Sort 2500")
  (newline)
  (time (selection-sort! V2500))
  (newline)
  (display "Selection Sort 3000")
  (newline)
  (time (selection-sort! V3000))
  (newline)
  (display "Selection Sort 3500")
  (newline)
  (time (selection-sort! V3500))
  (newline)
  (display "Selection Sort 4000")
  (newline)
  (time (selection-sort! V4000))
  (newline)
  (display "Selection Sort 4500")
  (newline)
  (time (selection-sort! V4500))
  (newline)
  (display "Selection Sort 5000")
  (newline)
  (time (selection-sort! V5000))
  (newline)
  (display "Selection Sort 5500")
  (newline)
  (time (selection-sort! V5500))
  (newline)
  (display "Selection Sort 6000")
  (newline)
  (time (selection-sort! V6000))
  (newline)
  (display "Selection Sort 6500")
  (newline)
  (time (selection-sort! V6500))
  (newline)
  (display "Selection Sort 7000")
  (newline)
  (time (selection-sort! V7000))
  (newline)
  (display "Selection Sort 7500")
  (newline)
  (time (selection-sort! V7500))
  (newline)
  (display "Selection Sort 8000")
  (newline)
  (time (selection-sort! V8000))
  (newline)
  (display "Selection Sort 8500")
  (newline)
  (time (selection-sort! V8500))
  (newline)
  (display "Selection Sort 9000")
  (newline)
  (time (selection-sort! V9000))
  (newline)
  (display "Selection Sort 9500")
  (newline)
  (time (selection-sort! V9500))
  (newline)
  (display "Selection Sort 10000")
  (newline)
  (time (selection-sort! V10000))
  (newline)
  (display "Selection Sort 10500")
  (newline)
  (time (selection-sort! V10500))
  (newline)
  (display "Selection Sort 11000")
  (newline)
  (time (selection-sort! V11000))
  (newline)
  (display "Selection Sort 11500")
  (newline)
  (time (selection-sort! V11500))
  (newline)
  (display "Selection Sort 12000")
  (newline)
  (time (selection-sort! V12000))
  (newline)
  (display "Selection Sort 12500")
  (newline)
  (time (selection-sort! V12500))
  (newline)
  (display "Selection Sort 13000")
  (newline)
  (time (selection-sort! V13000))
  (newline)
  (display "Selection Sort 13500")
  (newline)
  (time (selection-sort! V13500))
  (newline)
  (display "Selection Sort 14000")
  (newline)
  (time (selection-sort! V14000))
  (newline)
  (display "Selection Sort 14500")
  (newline)
  (time (selection-sort! V14500))
  (newline)
  (display "Selection Sort 15000")
  (newline)
  (time (selection-sort! V15000)))
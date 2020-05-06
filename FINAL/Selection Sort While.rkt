;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Selection Sort While|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "while.rkt")

; signature:
; Purpose:
; Effect:
#;(define (f-while ...)
  (local [ (define state-var1 (void))
           ...
           (define state-varN (void))]
    (begin
      (set! state-var1 ...)
      ...
      (set! state-varN ...)
      (while <driver>
             <while-body>)
      <return-value>)))

(define (empty-VINTV? low high) (> low high))
(define (empty-VINTV2? low high) (> low high))

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
          ; Purpose: For the given VINTV2, return the index of the smallest number
          ; Termination Argument: The design is based on structural recursion. [low...high]
          ; is valid for V. i starts at low and j starts at high. In each loop, i is increased
          ; by 1. Eventually, i becomes larger than j, terminating the loop. 
          (define (select low high)
            (local [; natnum
                    ; Purpose: the low index of the interval being processed
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
                ; INV: 
                (while (not (empty-VINTV2? i j))
                       (if (<= (vector-ref V i) (vector-ref V smallest))
                           (set! smallest i)
                           (void))
                       (set! i (add1 i))) ; close while 
                smallest))) 
          ; natnum
          ; Purpose: the low element of the interval being processed 
          (define low (void))
          ; natnum
          ; Purpose: the high element of the interval being processed 
          (define high (void))
          ]
    (begin
      (set! low 0)
      (set! high (sub1 (vector-length V)))
      (while (< low high)
             (if (> (vector-ref V low)
                    (vector-ref V (select (add1 low) high)))
                 (swap! low (select (add1 low) high))
                 (void))
             (set! low (add1 low)))
      )))


(define v (vector 4 2 9 10 7))

(check-expect (begin
                (selection-sort! v)
                v)
             (vector 2 4 7 9 10)) 

;; TIMING
(define (display-times x)
  (cond [(> x 15000) (void)]
        [else (begin
                (display (format "Selection Sort ~a" x))
                (newline)
                (time (selection-sort! (build-vector x (lambda (i) (random 1000000)))))
                (newline)
                (display-times (+ x 500)))]))

;(display-times 500) 
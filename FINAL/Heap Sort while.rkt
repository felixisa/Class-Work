;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Heap Sort while|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

;; HEAP SORT

; heap-sort!: (vectorof number) -> (void)
; Purpose: To sort in non-decreasing order the given array in place using heap sort
; Effect: The given array has it's elements reorganized to be sorted
(define (heap-sort! V)
  (local [; natnum natnum -> (void)
          ; Purpose: To swap V's ith and jth elements
          ; Effect: V[i] and V[j] are swapped
          (define (swap i j)
            (local [(define temp (vector-ref V i))]
              (begin
                (vector-set! V i (vector-ref V j))
                (vector-set! V j temp))))

          ; parent: natnum -> natnum
          ; Purpose: To return the parent index of the given index
          (define (parent i)
            (cond [(odd? i) (/ (sub1 i) 2)]
                  [else (/ (- i 2) 2)])) 

          ; heapify!: int int -> (void)
          ; Purpose: For the given VINTV, make the given vector a heap
          ; Effect: Reorganizes the vector elements to form a heap rooted at low
          ; Assumption: The given VINTV is valid for V and low > 0
          (define (heapify! low high)
            (local [(define j (void))]
              (begin
                (set! j high)
                (while (not (empty-VINTV? low j))
                       (local [(define parent-index (parent j))]
                         (if (> (vector-ref V parent-index) (vector-ref V j))
                             (set! j (sub1 j))
                             (begin (swap parent-index j)
                                    (trickle-down! j (sub1 (vector-length V)))
                                    (set! j (sub1 j))))))
                (void))))
          
          ; max-child-index: natnum natnum -> natnum
          ; Purpose: to return the given index with the largest vector value
          ; Assumption: The given indices are valid for V
          (define (max-child-index i j)
            (cond [(>= (vector-ref V i) (vector-ref V j)) i]
                  [else j]))
    
          ; trickle-down!: int int -> (void)
          ; Purpose: For the given VINTV, re-establish a heap rooted at low
          ; Effect: Vector elements are moved to have a heap rooted at low
          ; Assumption: The given VINTV is valid for V
          ; Termination Argument: The design is based on structural recursion.
          ; [i..j] is valid for V. i starts at low and j starts at high. Each time through the loop,
          ; i is changed to the index of its biggest child. Eventually, i increases to a number that
          ; has no children, terminating the loop. 
          (define (trickle-down! low high)
            (local [(define i (void))
                    (define j (void))
                    (define (left-child i) (add1 (* 2 i)))
                    (define (right-child i) (+ (* 2 i) 2))]
              (begin
                (set! i low)
                (set! j high)
                (while (and (not (> (left-child i) j)) ; i has children 
                           (not (<= (vector-ref V (left-child i)) ; i only has a left child and the heap relation doesn't exist
                                    (vector-ref V i))))
                      (if (> (right-child i) j) ; if the index of i's right child is bigger than j
                          (begin (swap i (left-child i)) (set! i (left-child i)))
                          (local [(define mc-index (max-child-index (left-child i) (right-child i)))] 
                                (begin ; re-establish heap relationship
                                  (swap i mc-index)
                                  (set! i mc-index)))))
                (void)))) 
          ; int
          ; Purpose: To maintain the high index 
          (define high (void))
          ]
    ; Termination Argument: The design is based on structural recursion.
    ; [0..high] is valid for V. high starts at (sub1 (vector-length V)). Each time through the loop high is
    ; decreased by one. Eventually, h becomes less than 0 and the loops terminates. 
    (begin
      (heapify! 1 (sub1 (vector-length V))) ; converts V into a heap such that each number is <= its root
      (set! high (sub1 (vector-length V))) ; initialize high to be the (lengthof V) - 1
      (while (not (empty-VINTV? 0 high)) ; while the high index is >= low, which stays at 0
             (swap 0 high) ; put largest element at end of given valid VINTV
             (trickle-down! 0 (sub1 high)) ; re-establish heap in rest of VINTV
             (set! high (sub1 high)))
      (void))))
             

(define V (vector 3 1 2))
(check-expect (begin
                (heap-sort! V)
                V)
              (vector 1 2 3))

(define V2 (vector 10 9 1 7 4 8))
(check-expect (begin
                (heap-sort! V2)
                V2)
              (vector 1 4 7 8 9 10))

(define V3 (vector 25 30 40 3 1 2))

(check-expect (begin
                (heap-sort! V3)
                V3)
              (vector 1 2 3 25 30 40))

;; TIMING

(define (display-times x)
  (cond [(> x 15000) (void)]
        [else (begin
                (display (format "Heap Sort ~a" x))
                (newline)
                (time (heap-sort! (build-vector x (lambda (i) (random 1000000)))))
                (newline)
                (display-times (+ x 500)))]))

(display-times 500) 
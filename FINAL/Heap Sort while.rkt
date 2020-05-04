;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Heap Sort while|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "while.rkt")
; f-on-vector: (vector X) ->
; Purpose:
#;(define (f-on-vector V)
    (local [; f-on-VINTV: int int ->
            ; Purpose: For the given VINTV, ...
            (define (f-on-VINTV low high)
              (cond [(empty-VINTV? low high) ...]
                    [else (vector-ref V high)...(f-on-VINTV low (sub1 high))]))
            ; f-on-VINTV2: int int ->
            ; Purpose: For the given VINTV2, ...
            (define (f-on-VINTV2 low high)
              (cond [(empty-VINTV2? low high) ...]
                    [else (vector-ref V low)...(f-on-VINTV2 (add1 low) high)]))]
      ...))

(define (empty-VINTV? low high) (> low high))
(define (empty-VINTV2? low high) (> low high))

;; HEAP SORT

; heap-sort!: (vectorof number) -> (void)
; Purpose: To sort in non-decreasing order the given array in
; place using heap sort
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
          (define (trickle-down! low high)
            (local [(define i (void))
                    (define j (void))]
              (begin
                (set! i low)
                (set! j high)
                (while(and (not (> (add1 (* 2 i)) j))
                              (not (<= (vector-ref V (add1 (* 2 i))) ; heap relationship exists
                                       (vector-ref V i))))
                         (if (> (+ (* 2 i) 2) j)
                             (begin (swap i (add1 (* 2 i))) (set! i (add1 (* 2 i))))
                             (local [(define mc-index (max-child-index (add1 (* 2 i)) (+ (* 2 i) 2)))]
                               (if (>= (vector-ref V i) (vector-ref V mc-index))
                                   (void); heap relation exists
                                   (begin ; re-establish heap relationship
                                     (swap i mc-index)
                                     (set! i mc-index))))))
                (void))))
          
          (define high (void))
          ]
    (begin
      (heapify! 1 (sub1 (vector-length V))) ; 0 has no parents → no need to heapify!
      (set! high (sub1 (vector-length V)))
      (while (not (empty-VINTV? 0 high))
             (swap 0 high) ; put largest element at end of given valid VINTV
             (trickle-down! 0 (sub1 high)) ; re-establish heap in rest of VINTV
             (set! high (sub1 high)))
      (void))))
             

(define V3 (vector 25 30 40 3 1 2))

(check-expect (begin
                (heap-sort! V3)
                V3)
              (vector 1 2 3 25 30 40))

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
#;(begin
    (display "Heap Sort 500")
    (newline)
    (time (heap-sort! V500))
    (newline)
    (display "Heap Sort 1000")
    (newline)
    (time (heap-sort! V1000))
    (newline)
    (display "Heap Sort 1500")
    (newline)
    (time (heap-sort! V1500))
    (newline)
    (display "Heap Sort 2000")
    (newline)
    (time (heap-sort! V2000))
    (newline)
    (display "Heap Sort 2500")
    (newline)
    (time (heap-sort! V2500))
    (newline)
    (display "Heap Sort 3000")
    (newline)
    (time (heap-sort! V3000))
    (newline)
    (display "Heap Sort 3500")
    (newline)
    (time (heap-sort! V3500))
    (newline)
    (display "Heap Sort 4000")
    (newline)
    (time (heap-sort! V4000))
    (newline)
    (display "Heap Sort 4500")
    (newline)
    (time (heap-sort! V4500))
    (newline)
    (display "Heap Sort 5000")
    (newline)
    (time (heap-sort! V5000))
    (newline)
    (display "Heap Sort 5500")
    (newline)
    (time (heap-sort! V5500))
    (newline)
    (display "Heap Sort 6000")
    (newline)
    (time (heap-sort! V6000))
    (newline)
    (display "Heap Sort 6500")
    (newline)
    (time (heap-sort! V6500))
    (newline)
    (display "Heap Sort 7000")
    (newline)
    (time (heap-sort! V7000))
    (newline)
    (display "Heap Sort 7500")
    (newline)
    (time (heap-sort! V7500))
    (newline)
    (display "Heap Sort 8000")
    (newline)
    (time (heap-sort! V8000))
    (newline)
    (display "Heap Sort 8500")
    (newline)
    (time (heap-sort! V8500))
    (newline)
    (display "Heap Sort 9000")
    (newline)
    (time (heap-sort! V9000))
    (newline)
    (display "Heap Sort 9500")
    (newline)
    (time (heap-sort! V9500))
    (newline)
    (display "Heap Sort 10000")
    (newline)
    (time (heap-sort! V10000))
    (newline)
    (display "Heap Sort 10500")
    (newline)
    (time (heap-sort! V10500))
    (newline)
    (display "Heap Sort 11000")
    (newline)
    (time (heap-sort! V11000))
    (newline)
    (display "Heap Sort 11500")
    (newline)
    (time (heap-sort! V11500))
    (newline)
    (display "Heap Sort 12000")
    (newline)
    (time (heap-sort! V12000))
    (newline)
    (display "Heap Sort 12500")
    (newline)
    (time (heap-sort! V12500))
    (newline)
    (display "Heap Sort 13000")
    (newline)
    (time (heap-sort! V13000))
    (newline)
    (display "Heap Sort 13500")
    (newline)
    (time (heap-sort! V13500))
    (newline)
    (display "Heap Sort 14000")
    (newline)
    (time (heap-sort! V14000))
    (newline)
    (display "Heap Sort 14500")
    (newline)
    (time (heap-sort! V14500))
    (newline)
    (display "Heap Sort 15000")
    (newline)
    (time (heap-sort! V15000)))
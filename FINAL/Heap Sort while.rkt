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
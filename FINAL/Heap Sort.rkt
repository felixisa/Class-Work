;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Heap Sort|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
  (local [; parent: natnum -> natnum
          ; Purpose: To return the parent index of the given index
          (define (parent i)
            (cond [(odd? i) (/ (sub1 i) 2)]
                  [else (/ (- i 2) 2)])) 

          ; heapify!: int int -> (void)
          ; Purpose: For the given VINTV, make the given vector a heap
          ; Effect: Reorganizes the vector elements to form a heap rooted at low
          ; Assumption: The given VINTV is valid for V and low > 0
          (define (heapify! low high)
            (cond [(empty-VINTV? low high) (void)] ; There is nothing to heapify
                  [else
                   (local [(define parent-index (parent high))] ; parent index in VINTV
                     (cond [(> (vector-ref V parent-index) (vector-ref V high))
                            (heapify! low (sub1 high))] ; make the rest of the VINTV a heap
                           [else (begin (swap parent-index high)
                                        (trickle-down! high (sub1 (vector-length V)))
                                        (heapify! low (sub1 high)))]))]))

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
            (local [(define rc-index (+ (* 2 low) 2)) ; right child index
                    (define lc-index (add1 (* 2 low)))] ; left child index
              (cond [(> lc-index high) (void)] ; low has no children
                    [(> rc-index high) ; low only has a left child
                     (cond [(<= (vector-ref V lc-index) ; heap relationship exists
                                (vector-ref V low))
                            (void)]
                           [else
                            (begin
                              (swap low lc-index) ; put parent and child in right order
                              (trickle-down! lc-index high))])]
                    [else (local [(define mc-index (max-child-index lc-index rc-index))]
                            (cond [(>= (vector-ref V low) (vector-ref V mc-index)) (void)]
                                  ; heap relation exists
                                  [else (begin ; re-establish heap relationship
                                          (swap low mc-index)
                                          (trickle-down! mc-index high))]))])))

          ; sort!: int int -> (void)
          ; Purpose: For the given VINTV, sort the vector elements
          ; Effect: Vector elements in the given VINTV rearranged in non-decreasing order
          ; Assumption: V is a heap and the given VINTV is valid for V
          (define (sort! low high)
            (cond [(empty-VINTV? low high) (void)] ; there is nothing to sort
                  [else (begin
                          (swap low high) ; put largest element at end of given valid VINTV
                          (trickle-down! low (sub1 high)) ; re-establish heap in rest of VINTV
                          (sort! low (sub1 high)))])) ; sort the rest of the interval

          ; natnum natnum -> (void)
          ; Purpose: To swap V's ith and jth elements
          ; Effect: V[i] and V[j] are swapped
          (define (swap i j)
            (local [(define temp (vector-ref V i))]
              (begin
                (vector-set! V i (vector-ref V j))
                (vector-set! V j temp))))
          ]
    (begin
      (heapify! 1 (sub1 (vector-length V))) ; 0 has no parents → no need to heapify!
      (sort! 0 (sub1 (vector-length V))))))

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
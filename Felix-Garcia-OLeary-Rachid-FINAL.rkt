;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Felix-Garcia-OLeary-Rachid-FINAL) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Final Exam Sorting Algorithms

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

;; INSERTION SORT

; insort-in-place!: (vector number) -> (void)
; Purpose: To sort the given vector in non-decreasing order
; Effect: To rearrange the elements of the given vector in non-decreasing order
(define (insort-in-place! V)
  (local [(define (swap i j)
            (local [(define temp (vector-ref V i))]
              (begin
                (vector-set! V i (vector-ref V j))
                (vector-set! V j temp))))
          ; insert!: VINTV2(natnum natnum) -> (void)
          ; Purpose: For the given VINTV2, insert V[low] in V[low+1..high]
          ; such that V[low..high] is in non-decreasing order
          ; Effect: V elements are swapped until one is >= V[low] or
          ; the given VINTV2 is empty
          (define (insert! low high)
            (cond [(empty-VINTV2? low high) (void)]
                  [else (cond [(<= (vector-ref V low)
                                   (vector-ref V (add1 low)))
                               (void)]
                              [else (begin (swap low (add1 low))
                                           (insert! (add1 low) high))])]))

          ; sort!: VINTV2(natnum natnum) -> (void)
          ; Purpose: For the given VINTV2, sort V using insertion sort
          ; Effect: Rearrange V elements in the given VINTV2 in non-decreasing order
          (define (sort! low high)
            (cond [(empty-VINTV2? low high) (void)]
                  [else (begin
                          (sort! (add1 low) high)
                          (insert! low (sub1 high)))]))]
    
    (sort! 0 (sub1 (vector-length V)))))

(define V1 (vector 10 3 7 17 11))

(check-expect (begin
                (insort-in-place! V1)
                V1)
              (vector 3 7 10 11 17))

;; QUICK SORT

; qs-in-place!: (vectorof number) -> (void)
; Purpose: To sort the array in non-decreasing order.
; Effect: The elements of the array are rearranged in place.
(define (qs-in-place! V)
  (local [; partition!: VINTV(natnum natnum) natnum -> number
          ; Purpose: For the given VINTV, partition E& place pivot in final position.
          ; Effect: Mutate V so that all elements before the pivot are
          ; <= pivot and all elements after the pivot are > pivot.
          (define (partition! low high pp)
            (local
              [; swap: natnum natnum -> (void)
               ; Purpose: To swap V[i] and V[j]
               ; Effect: Modify V[i] to contain the value of V[j] and vice versa
               (define (swap i j)
                 (local [(define temp (vector-ref V i))]
                   (begin
                     (vector-set! V i (vector-ref V j))
                     (vector-set! V j temp))))
               ; small-index: VINTV(natnum natnum) natnum -> natnum
               ; Purpose: For the given VINTV, find largest index: V[k] <= pivot
               (define (small-index low high pivot)
                 (cond [(empty-VINTV? low high) low]
                       [else (cond [(<= (vector-ref V high) pivot) high]
                                   [else (small-index low (sub1 high) pivot)])]))
               ; larger-index: VINTV2(natnum natnum) natnum -> natnum 
               ; Purpose: For the given VINTV2, find smallest index: V[k] > pivot else return high
               (define (larger-index low high pivot)
                 (cond [(empty-VINTV2? low high) high]
                       [else (cond [(> (vector-ref V low) pivot) low]
                                   [else (larger-index (add1 low) high pivot)])]))
               ; separate!: VINTV(natnum natnum) natnum -> natnum
               ; Purpose: For the given VINTV, separate smaller and larger elements
               ; Effect: In V move elements <= pivot before elements > pivot.
               (define (separate! low high pp)
                 (local [(define s-index (small-index low high (vector-ref V pp)))
                         (define l-index (larger-index low high (vector-ref V pp)))]
                   (cond [(<= s-index l-index) s-index]
                         [else (begin (swap s-index l-index)
                                      (separate! l-index s-index pp))])))]
              (begin
                (local [(define pivot-pos (separate! low high low))]
                  (begin (swap pp pivot-pos) pivot-pos)))))
          ; qs-aux!: VINTV(natnum natnum) -> (void)
          ; Purpose: For the given VINTV, sort V in non-decreasing order.
          ; Effect: The elements in the given interval are rearranged in place.
          (define (qs-aux! low high)
            (cond [(empty-VINTV? low high) (void)]
                  [else (local [(define pp (partition! low high low))]
                          (begin (qs-aux! low (sub1 pp)) (qs-aux! (add1 pp) high)))]))]
    (qs-aux! 0 (sub1 (vector-length V)))))

(define V2 (vector 10 34 3 8 27 14 9 31 7 87))

(check-expect (begin
                (qs-in-place! V2)
                V2)
              (vector 3 7 8 9 10 14 27 31 34 87))

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

;; RADIX/BUCKET SORT

;A bucket has a (vectorof X) and the index of the next available position 
;in the vector. A bucket is an interface that offers:
;
;1. (bucket-add! n): puts n in the next spot available in the bucket
;2. (bucket-dump! D i): empties the bucket and puts all its elements in
;vector D starting at i. Emptying the bucket returns all
;elements to (void) and the number of elements to 0.
;3. (bucket-size): returns the number of elements in the bucket
;4. (bucket-elems): returns the array of elements in the bucket
;
;(make-bucket k): creates a bucket that can hold k elements. Initially, all k
;elements are (void) and the number of elements in the bucket is zero.

; make-bucket: number → bucket
; Purpose: To make a bucket of the given size
(define (make-bucket n)
  (local [; (vector of number)
          ; Purpose: To hold the numbers in the bucket
          (define V (build-vector n (lambda (i) (void))))
          
          ; natnum
          ; Purpose: The index of the next spot available in the bucket (size of the bucket)
          (define i 0)
          
          ; natnum → (void)
          ; Purpose: To add the given number to the bucket
          ; Effect: V[i] gets the value of n and i in increased
          (define (add! n)
            (begin
              (vector-set! V i n)
              (set! i (add1 i))))

          ; (vectorof number) natnum → (void)
          ; Purpose: For the given VINTV2s, put all bucket elements in the given vector 
          ; Effect: i made 0; All bucket elems made void; D[j..bucketsize-1]=bucket elems
          (define (dump! D j)
            (local [(define (helper dlow dhigh vlow vhigh)
                      (cond [(empty-VINTV2? vlow vhigh) (void)]
                            [else (cond [(empty-VINTV2? dlow dhigh)
                                         (error "Dumping vector is too small")]
                                        [else (begin
                                                (vector-set! D dlow (vector-ref V vlow))
                                                (vector-set! V vlow (void))
                                                (helper (add1 dlow) dhigh
                                                        (add1 vlow) vhigh))])]))]
              (begin
                (helper j (sub1 (vector-length D)) 0 (sub1 i))
                (set! i 0))))

          ; Purpose: to manage the adding, dumping, inquiry of size,
          ; and inquiry of elements of a bucket
          (define (service-manager m)
            (cond [(eq? m 'add) add!]
                  [(eq? m 'dump) dump!]
                  [(eq? m 'size) i]
                  [(eq? m 'elems) V]
                  [else (error 'bucket "Unknown message.")]))]
    service-manager))

; Bucket Wrap Functions

; bucket-add!: bucket number -> (void)
; Purpose: To add the given number to the bucket 
(define (bucket-add! B v)
  ((B 'add) v))

; bucket-dump!: bucket (vectorof num) natnum -> (void)
; Purpose: To put all bucket elements into the given vector
; starting at the given index
(define (bucket-dump! B D i)
  ((B 'dump) D i))

; bucket-size: bucket -> natnum
; Purpose: returns the next available spot in the bucket
(define (bucket-size B)
  (B 'size))

; bucket-elems: bucket -> (vectorof num)
; Purpose: returns the current vector of the bucket elements 
(define (bucket-elems B)
  (B 'elems))

(define B (make-bucket 10))
(define D (build-vector 10 (lambda (i) 0)))

(check-expect (begin
                (bucket-add! B 0)
                (bucket-add! B 1)
                (bucket-add! B 2)
                (bucket-add! B 3)
                (bucket-dump! B D 0)
                D)
              (vector 0 1 2 3 0 0 0 0 0 0))
(check-expect (bucket-size B) 0)
(check-expect (bucket-elems B) (vector
                                (void)
                                (void)
                                (void)
                                (void)
                                (void)
                                (void)
                                (void)
                                (void)
                                (void)
                                (void)))

;; SELECTION SORT 

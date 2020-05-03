;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Radix Sort but it looks wrong|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

; radix-sort!: (vectorof number) -> (void)
; Purpose: To sort the given vector in non-decreasing order
; Effect: To rearrange the elements of the given vector in non-decreasing order
(define (radix-sort! V)
  (local [(define b-vect (build-vector 10 (lambda (i) (make-bucket (vector-length V)))))
          ; natnum
          ; Since radix sort takes n steps, the length of the vector is the amount
          ; of steps that it should take to sort the vector
          (define n (void))
          ; natnum
          ; Purpose: To count the amount of steps so far
          (define count (void))
          ; natnum
          ; Purpose: To keep track of the digit to bucketize
          (define digit (void))
          ; bucketize: (vectorof number) number -> (void)
          ; Purpose: To bucketize the elements of a vector based on the current digit
          (define (bucketize low dig)
                      (if (> low (sub1 (vector-length V))) (void) 
                          (begin (bucket-add!
                                  (vector-ref b-vect (modulo (floor (/ (vector-ref V low) dig)) 10))
                                  (vector-ref V low))
                                 (bucketize (add1 low) dig))))

          ; debucketize: (vectorof number) (vectorof bucket) -> (void) 
          ; Purpose: To dump each bucket of the vector of buckets into the vector of number in order 
          (define (debucketize bvlow d-index)
            (if (empty-VINTV? bvlow (sub1 (vector-length b-vect)))
                (void)
                (local [(define new-d-index (+ (bucket-size (vector-ref b-vect bvlow)) d-index))]
                  (begin
                    (bucket-dump! (vector-ref b-vect bvlow) V d-index)
                    (debucketize (add1 bvlow) new-d-index)))))

          ; most-dig: (vectorof number) -> number
          ; Purpose: To find the length of the largest number in a vector
          (define (most-dig v)
            (local [; most-dig-help: natnum natnum number -> number
                    ; Purpose:
                    ; accum is the largest number so far
                    (define (most-dig-help low high accum)
                      (cond [(empty-VINTV? low high) accum]
                            [(> (vector-ref v low) accum) (most-dig-help (add1 low) high (vector-ref v low))]
                           [else (most-dig-help (add1 low) high accum)]))
                    ; count-digits: num -> num
                    ; Purpose: Count the number of digits in a number
                    (define(count-digits num accum)
                      (if (< num 10) (+ 1 accum)
                          (count-digits (/ num 10) (+ 1 accum))))]
              (count-digits (most-dig-help 0 (sub1 (vector-length v)) 0) 0)))
                    
          ] 
    (begin
      (set! count 1)
      (set! n (most-dig V))
      (set! digit 1)
      (while (not (> count n))
             (bucketize 0 digit)
             (debucketize 0 0)
             (set! count (add1 count))
             (set! digit (* digit 10))
             )
      (void))))

(define v (vector 989 82 67 50 103 4))
(check-expect (begin
                (radix-sort! v)
                v)
              (vector 4 50 67 82 103 989))

(define v1 (vector 677 577 777 377))
(check-expect (begin
                (radix-sort! v1)
                v1)
              (vector 377 577 677 777))

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
  (display "Radix Sort 500")
  (newline)
  (time (radix-sort! V500))
  (newline)
  (display "Radix Sort 1000")
  (newline)
  (time (radix-sort! V1000))
  (newline)
  (display "Radix Sort 1500")
  (newline)
  (time (radix-sort! V1500))
  (newline)
  (display "Radix Sort 2000")
  (newline)
  (time (radix-sort! V2000))
  (newline)
  (display "Radix Sort 2500")
  (newline)
  (time (radix-sort! V2500))
  (newline)
  (display "Radix Sort 3000")
  (newline)
  (time (radix-sort! V3000))
  (newline)
  (display "Radix Sort 3500")
  (newline)
  (time (radix-sort! V3500))
  (newline)
  (display "Radix Sort 4000")
  (newline)
  (time (radix-sort! V4000))
  (newline)
  (display "Radix Sort 4500")
  (newline)
  (time (radix-sort! V4500))
  (newline)
  (display "Radix Sort 5000")
  (newline)
  (time (radix-sort! V5000))
  (newline)
  (display "Radix Sort 5500")
  (newline)
  (time (radix-sort! V5500))
  (newline)
  (display "Radix Sort 6000")
  (newline)
  (time (radix-sort! V6000))
  (newline)
  (display "Radix Sort 6500")
  (newline)
  (time (radix-sort! V6500))
  (newline)
  (display "Radix Sort 7000")
  (newline)
  (time (radix-sort! V7000))
  (newline)
  (display "Radix Sort 7500")
  (newline)
  (time (radix-sort! V7500))
  (newline)
  (display "Radix Sort 8000")
  (newline)
  (time (radix-sort! V8000))
  (newline)
  (display "Radix Sort 8500")
  (newline)
  (time (radix-sort! V8500))
  (newline)
  (display "Radix Sort 9000")
  (newline)
  (time (radix-sort! V9000))
  (newline)
  (display "Radix Sort 9500")
  (newline)
  (time (radix-sort! V9500))
  (newline)
  (display "Radix Sort 10000")
  (newline)
  (time (radix-sort! V10000))
  (newline)
  (display "Radix Sort 10500")
  (newline)
  (time (radix-sort! V10500))
  (newline)
  (display "Radix Sort 11000")
  (newline)
  (time (radix-sort! V11000))
  (newline)
  (display "Radix Sort 11500")
  (newline)
  (time (radix-sort! V11500))
  (newline)
  (display "Radix Sort 12000")
  (newline)
  (time (radix-sort! V12000))
  (newline)
  (display "Radix Sort 12500")
  (newline)
  (time (radix-sort! V12500))
  (newline)
  (display "Radix Sort 13000")
  (newline)
  (time (radix-sort! V13000))
  (newline)
  (display "Radix Sort 13500")
  (newline)
  (time (radix-sort! V13500))
  (newline)
  (display "Radix Sort 14000")
  (newline)
  (time (radix-sort! V14000))
  (newline)
  (display "Radix Sort 14500")
  (newline)
  (time (radix-sort! V14500))
  (newline)
  (display "Radix Sort 15000")
  (newline)
  (time (radix-sort! V15000)))

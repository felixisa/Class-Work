;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Radix Sort While|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

;; TIMING

(define (display-times x)
  (cond [(> x 15000) (void)]
        [else (begin
                (display (format "Radix Sort ~a" x))
                (newline)
                (time (radix-sort! (build-vector x (lambda (i) (random 1000000)))))
                (newline)
                (display-times (+ x 500)))]))

(display-times 500) 
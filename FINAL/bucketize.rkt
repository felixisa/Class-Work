;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname bucketize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (empty-VINTV? low high) (> low high))
(define (empty-VINTV2? low high) (> low high))

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

(define b-vect (build-vector 10 (lambda (i) (make-bucket 10))))

; debucketize: (vectorof number) (vectorof bucket) -> (void) 
; Purpose: To dump each bucket of the vector of buckets into the vector of number in order 
(define (debucketize v bv)
  (local [(define (debuck-help bvlow bvhigh d-index)
            (if (empty-VINTV? bvlow bvhigh) (void)
                (begin
                  (bucket-dump! (vector-ref bv bvlow) v d-index)
                  (debuck-help (add1 bvlow) bvhigh (+ (bucket-size (vector-ref bv bvlow)) d-index)))))]
    (debuck-help 0 (sub1 (vector-length bv)) 0)))

; bucketize: (vectorof number) number -> (void)
; Purpose: To bucketize the elements of a vector based on the current digit
(define (bucketize v dig)
  (local [(define (bucket-help low high dig)
            (if (> low high) (void) 
                (begin (bucket-add!
                        (vector-ref b-vect (modulo (floor (/ (vector-ref v low) dig)) 10))
                        (vector-ref v low))
                       (bucket-help (add1 low) high dig))))]
    (bucket-help 0 (sub1 (vector-length v)) dig)))
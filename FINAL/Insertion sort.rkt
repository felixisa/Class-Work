;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Insertion sort|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

; insort-while!: (vector number) -> (void)
; Purpose: To sort the given vector in non-decreasing order
; Effect: To rearrange the elements of the given vector in non-decreasing order
(define (insort-while! V)
  (local [; swap: natnum natnum -> (void)
          ; Purpose: To swap V[i] and V[j]
          ; Effect: Modify V[i] to contain the value of V[j] and vice versa
          (define (swap i j)
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
  (display "Insertion Sort 500")
  (newline)
  (time (insort-in-place! V500))
  (newline)
  (display "Insertion Sort 1000")
  (newline)
  (time (insort-in-place! V1000))
  (newline)
  (display "Insertion Sort 1500")
  (newline)
  (time (insort-in-place! V1500))
  (newline)
  (display "Insertion Sort 2000")
  (newline)
  (time (insort-in-place! V2000))
  (newline)
  (display "Insertion Sort 2500")
  (newline)
  (time (insort-in-place! V2500))
  (newline)
  (display "Insertion Sort 3000")
  (newline)
  (time (insort-in-place! V3000))
  (newline)
  (display "Insertion Sort 3500")
  (newline)
  (time (insort-in-place! V3500))
  (newline)
  (display "Insertion Sort 4000")
  (newline)
  (time (insort-in-place! V4000))
  (newline)
  (display "Insertion Sort 4500")
  (newline)
  (time (insort-in-place! V4500))
  (newline)
  (display "Insertion Sort 5000")
  (newline)
  (time (insort-in-place! V5000))
  (newline)
  (display "Insertion Sort 5500")
  (newline)
  (time (insort-in-place! V5500))
  (newline)
  (display "Insertion Sort 6000")
  (newline)
  (time (insort-in-place! V6000))
  (newline)
  (display "Insertion Sort 6500")
  (newline)
  (time (insort-in-place! V6500))
  (newline)
  (display "Insertion Sort 7000")
  (newline)
  (time (insort-in-place! V7000))
  (newline)
  (display "Insertion Sort 7500")
  (newline)
  (time (insort-in-place! V7500))
  (newline)
  (display "Insertion Sort 8000")
  (newline)
  (time (insort-in-place! V8000))
  (newline)
  (display "Insertion Sort 8500")
  (newline)
  (time (insort-in-place! V8500))
  (newline)
  (display "Insertion Sort 9000")
  (newline)
  (time (insort-in-place! V9000))
  (newline)
  (display "Insertion Sort 9500")
  (newline)
  (time (insort-in-place! V9500))
  (newline)
  (display "Insertion Sort 10000")
  (newline)
  (time (insort-in-place! V10000))
  (newline)
  (display "Insertion Sort 10500")
  (newline)
  (time (insort-in-place! V10500))
  (newline)
  (display "Insertion Sort 11000")
  (newline)
  (time (insort-in-place! V11000))
  (newline)
  (display "Insertion Sort 11500")
  (newline)
  (time (insort-in-place! V11500))
  (newline)
  (display "Insertion Sort 12000")
  (newline)
  (time (insort-in-place! V12000))
  (newline)
  (display "Insertion Sort 12500")
  (newline)
  (time (insort-in-place! V12500))
  (newline)
  (display "Insertion Sort 13000")
  (newline)
  (time (insort-in-place! V13000))
  (newline)
  (display "Insertion Sort 13500")
  (newline)
  (time (insort-in-place! V13500))
  (newline)
  (display "Insertion Sort 14000")
  (newline)
  (time (insort-in-place! V14000))
  (newline)
  (display "Insertion Sort 14500")
  (newline)
  (time (insort-in-place! V14500))
  (newline)
  (display "Insertion Sort 15000")
  (newline)
  (time (insort-in-place! V15000))) 
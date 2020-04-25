;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Quick sort|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
  (display "Quick Sort 500")
  (newline)
  (time (qs-in-place! V500))
  (newline)
  (display "Quick Sort 1000")
  (newline)
  (time (qs-in-place! V1000))
  (newline)
  (display "Quick Sort 1500")
  (newline)
  (time (qs-in-place! V1500))
  (newline)
  (display "Quick Sort 2000")
  (newline)
  (time (qs-in-place! V2000))
  (newline)
  (display "Quick Sort 2500")
  (newline)
  (time (qs-in-place! V2500))
  (newline)
  (display "Quick Sort 3000")
  (newline)
  (time (qs-in-place! V3000))
  (newline)
  (display "Quick Sort 3500")
  (newline)
  (time (qs-in-place! V3500))
  (newline)
  (display "Quick Sort 4000")
  (newline)
  (time (qs-in-place! V4000))
  (newline)
  (display "Quick Sort 4500")
  (newline)
  (time (qs-in-place! V4500))
  (newline)
  (display "Quick Sort 5000")
  (newline)
  (time (qs-in-place! V5000))
  (newline)
  (display "Quick Sort 5500")
  (newline)
  (time (qs-in-place! V5500))
  (newline)
  (display "Quick Sort 6000")
  (newline)
  (time (qs-in-place! V6000))
  (newline)
  (display "Quick Sort 6500")
  (newline)
  (time (qs-in-place! V6500))
  (newline)
  (display "Quick Sort 7000")
  (newline)
  (time (qs-in-place! V7000))
  (newline)
  (display "Quick Sort 7500")
  (newline)
  (time (qs-in-place! V7500))
  (newline)
  (display "Quick Sort 8000")
  (newline)
  (time (qs-in-place! V8000))
  (newline)
  (display "Quick Sort 8500")
  (newline)
  (time (qs-in-place! V8500))
  (newline)
  (display "Quick Sort 9000")
  (newline)
  (time (qs-in-place! V9000))
  (newline)
  (display "Quick Sort 9500")
  (newline)
  (time (qs-in-place! V9500))
  (newline)
  (display "Quick Sort 10000")
  (newline)
  (time (qs-in-place! V10000))
  (newline)
  (display "Quick Sort 10500")
  (newline)
  (time (qs-in-place! V10500))
  (newline)
  (display "Quick Sort 11000")
  (newline)
  (time (qs-in-place! V11000))
  (newline)
  (display "Quick Sort 11500")
  (newline)
  (time (qs-in-place! V11500))
  (newline)
  (display "Quick Sort 12000")
  (newline)
  (time (qs-in-place! V12000))
  (newline)
  (display "Quick Sort 12500")
  (newline)
  (time (qs-in-place! V12500))
  (newline)
  (display "Quick Sort 13000")
  (newline)
  (time (qs-in-place! V13000))
  (newline)
  (display "Quick Sort 13500")
  (newline)
  (time (qs-in-place! V13500))
  (newline)
  (display "Quick Sort 14000")
  (newline)
  (time (qs-in-place! V14000))
  (newline)
  (display "Quick Sort 14500")
  (newline)
  (time (qs-in-place! V14500))
  (newline)
  (display "Quick Sort 15000")
  (newline)
  (time (qs-in-place! V15000)))
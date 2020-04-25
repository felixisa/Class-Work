;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Insort while|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "while.rkt")

(define (empty-VINTV? low high) (> low high))
(define (empty-VINTV2? low high) (> low high))

; insort-while!: (vectorof number) -> (void)
; Purpose: To sort the given vector in non decreasing order
; Effect: The elements of the vector will be rearranged in place
(define (insort-while! V)
  (local [ ; swap: natnum natnum -> (void)
          ; Purpose: To swap V[i] and V[j]
          ; Effect: Modify V[i] to contain the value of V[j] and vice versa
          (define (swap! i j)
            (local [(define temp (vector-ref V i))]
              (begin
                (vector-set! V i (vector-ref V j))
                (vector-set! V j temp))))

          ; insert!: int int -> (void)
          ; Purpose: For the given VINTV2, insert V[low] in V[low+1..high]
          ; such that V[low..high] is in non-decreasing order
          ; Effect: V elements are swapped until one is >= V[low] or
          ; the given VINTV2 is empty
          ; Termination Argument: The design is based on structural recursion.
          ; [lo..hi] is valid for V. l starts at lo. Each time through the loop l is
          ; increased by one. Eventually, l becomes higher than hi and the loops
          ; terminates or V[l] <= V[l+1] and the loop terminates.
          (define (insert! lo hi)
            (local [(define l (void))]
              (begin
                (set! l lo)
                ; INV: l <= hi+1 & V[lo..l-1] elements <= V[l+1..hi]
                ; V[l+1..hi] sorted non-decresing & V[lo..l-1] sorted non-decresing
                (while (and (not (empty-VINTV? l hi))(> (vector-ref V l) (vector-ref V (add1 l))))
                       (begin
                         (swap! l (add1 l))
                         ; l <= hi & V[l+2..hi] is sorted in non-decresing order &
                         ; V[l+2..hi] sorted non-decreasing & V[l] < V[l+1] &
                         ; V[lo..l] elements <= V[l+2..hi] AND
                         ; V[lo..l] elements <= V[l+2..hi] AND
                         (set! l (add1 l))
                         ; INV l <= hi+1 AND
                         ; V[l+1..hi] is sorted in non-decresing order AND
                         ; V[lo..l-1] is sorted in non-decresing order AND
                         ; V[lo..l-1] elements <= V[l+1..hi]
                         ) ; closes while
                       ; ...
                       (void)))))
          ; l <= hi & V[l] > V[l+1]
          ; V[l+1..hi] sorted non-decreasing & V[lo..l-1] elements <= V[l+1..hi]
          ; V[lo..l-1] is sorted in non-decresing
                         

          ; sort!: VINTV_V[natnum,natnum] --> (void)
          ; Purpose: To sort the given vector interval in non-decreasing order
          ; Effect: The elements in the given interval are rearranged in-place
          ; Termination Argument: The design is based on structural recursion.
          ; [low..high] is valid for V. h starts at high. Each time through the loop h is
          ; decreased by one. Eventually, h becomes lower than low and the loops terminates
          (define (sort! low high)
            (local [ ; int
                    ; Purpose: Next element index to be moved to sorted part of V
                    (define h (void))]
              (begin
                (set! h high)
                ; INV: V[h+1..high] is sorted in non-decreasing order & h >= low-1
                (while (not (empty-VINTV? low h))
                       ; h >= low & V[h+1..high] is sorted in non-decreasing order
                       (insert! h (sub1 high))
                       ; h >= low AND V[h..high] is sorted in non-decreasing order
                       (set! h (sub1 h))
                       ; h >= low-1 & V[h+1..high] sorted in non-decreasing order)
                       ; V[h+1..high] sorted non-decreasing order & h >= low-1 & [low..h] is empty
                       ; ==> h < low ==> h = low-1
                       ; ==> V[low..high] is sorted in non-decreasing order
                       (void)))))
            ]
(sort! 0 (sub1 (vector-length V)))))

(define V (vector 3 1 2))
(check-expect (begin
                (insort-while! V)
                V)
              (vector 1 2 3))

(define V2 (vector 10 9 1 7 4 8))
(check-expect (begin
                (insort-while! V2)
                V2)
              (vector 1 4 7 8 9 10))

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
  (time (insort-while! V500))
  (newline)
  (display "Insertion Sort 1000")
  (newline)
  (time (insort-while! V1000))
  (newline)
  (display "Insertion Sort 1500")
  (newline)
  (time (insort-while! V1500))
  (newline)
  (display "Insertion Sort 2000")
  (newline)
  (time (insort-while! V2000))
  (newline)
  (display "Insertion Sort 2500")
  (newline)
  (time (insort-while! V2500))
  (newline)
  (display "Insertion Sort 3000")
  (newline)
  (time (insort-while! V3000))
  (newline)
  (display "Insertion Sort 3500")
  (newline)
  (time (insort-while! V3500))
  (newline)
  (display "Insertion Sort 4000")
  (newline)
  (time (insort-while! V4000))
  (newline)
  (display "Insertion Sort 4500")
  (newline)
  (time (insort-while! V4500))
  (newline)
  (display "Insertion Sort 5000")
  (newline)
  (time (insort-while! V5000))
  (newline)
  (display "Insertion Sort 5500")
  (newline)
  (time (insort-while! V5500))
  (newline)
  (display "Insertion Sort 6000")
  (newline)
  (time (insort-while! V6000))
  (newline)
  (display "Insertion Sort 6500")
  (newline)
  (time (insort-while! V6500))
  (newline)
  (display "Insertion Sort 7000")
  (newline)
  (time (insort-while! V7000))
  (newline)
  (display "Insertion Sort 7500")
  (newline)
  (time (insort-while! V7500))
  (newline)
  (display "Insertion Sort 8000")
  (newline)
  (time (insort-while! V8000))
  (newline)
  (display "Insertion Sort 8500")
  (newline)
  (time (insort-while! V8500))
  (newline)
  (display "Insertion Sort 9000")
  (newline)
  (time (insort-while! V9000))
  (newline)
  (display "Insertion Sort 9500")
  (newline)
  (time (insort-while! V9500))
  (newline)
  (display "Insertion Sort 10000")
  (newline)
  (time (insort-while! V10000))
  (newline)
  (display "Insertion Sort 10500")
  (newline)
  (time (insort-while! V10500))
  (newline)
  (display "Insertion Sort 11000")
  (newline)
  (time (insort-while! V11000))
  (newline)
  (display "Insertion Sort 11500")
  (newline)
  (time (insort-while! V11500))
  (newline)
  (display "Insertion Sort 12000")
  (newline)
  (time (insort-while! V12000))
  (newline)
  (display "Insertion Sort 12500")
  (newline)
  (time (insort-while! V12500))
  (newline)
  (display "Insertion Sort 13000")
  (newline)
  (time (insort-while! V13000))
  (newline)
  (display "Insertion Sort 13500")
  (newline)
  (time (insort-while! V13500))
  (newline)
  (display "Insertion Sort 14000")
  (newline)
  (time (insort-while! V14000))
  (newline)
  (display "Insertion Sort 14500")
  (newline)
  (time (insort-while! V14500))
  (newline)
  (display "Insertion Sort 15000")
  (newline)
  (time (insort-while! V15000)))
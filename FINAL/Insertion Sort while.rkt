;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Insertion Sort while|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "while.rkt")

; signature:
; Purpose:
; Effect:
#;(define (f-while ...)
  (local [ (define state-var1 (void))
           ...
           (define state-varN (void))]
    (begin
      (set! state-var1 ...)
      ...
      (set! state-varN ...)
      (while <driver>
             <while-body>)
      <return-value>)))

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
                (while (and (not (empty-VINTV? l hi)) (> (vector-ref V l) (vector-ref V (add1 l))))
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
          
          ; sort!: VINTV[natnum,natnum] --> (void)
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
                       ; h >= low and V[h+1..high] is sorted in non-decreasing order
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

;; TIMING

(define (display-times x)
  (cond [(> x 15000) (void)]
        [else (begin
                (display (format "Insertion Sort ~a" x))
                (newline)
                (time (insort-while! (build-vector x (lambda (i) (random 1000000)))))
                (newline)
                (display-times (+ x 500)))]))

(display-times 500) 
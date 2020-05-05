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


;; TIMING

(define (display-times x)
  (cond [(> x 15000) (void)]
        [else (begin
                (display (format "Insertion Sort ~a" x))
                (newline)
                (time (insort-in-place! (build-vector x (lambda (i) (random 1000000)))))
                (newline)
                (display-times (+ x 500)))]))

(display-times 500) 
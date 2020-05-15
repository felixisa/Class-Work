;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Selection Sort|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; selection-sort!: (vectorof number) -> (void)
; Purpose: To sort the array in non-decreasing order.
; Effect: The elements of the array are rearranged in place.
(define (selection-sort! V)
  (local [ ; select: natnum natnum -> natnum
          ; Purpose: To return the index of the smallest element in the vector
          ; ACCUM INV: Accum is the index of the smallest element of the processed interval
          (define (select low high accum)
            (cond [(> low high) accum]
                  [(< (vector-ref V low) (vector-ref V accum)) (select (add1 low) high low)]
                  [else (select (add1 low) high accum)]))

          ; sort!: natnum natnum -> (void)
          ; Purpose: For the given VINTV, sort V in non-decreasing order.
          ; Effect: The elements in the given interval are rearranged in place.
          (define (sort! low high)
            (cond [(> low high) (void)]
                  [else (begin (swap! low (select (add1 low) high low))
                               (sort! (add1 low) high))]))

          ; swap: natnum natnum -> (void)
          ; Purpose: To swap V[i] and V[j]
          ; Effect: Modify V[i] to contain the value of V[j] and vice versa
          (define (swap! i j)
            (local [(define temp (vector-ref V i))]
              (begin
                (vector-set! V i (vector-ref V j))
                (vector-set! V j temp))))
          ]
    (sort! 0 (sub1 (vector-length V)))))

(define v (vector 989 82 67 50 103 4))
(check-expect (begin
                (selection-sort! v)
                v)
              (vector 4 50 67 82 103 989))

(define v1 (vector 677 577 777 377))
(check-expect (begin
                (selection-sort! v1)
                v1)
              (vector 377 577 677 777))

(define (display-times x)
  (cond [(> x 15000) (void)]
        [else (begin
                (display (format "Selection Sort ~a" x))
                (newline)
                (time (selection-sort! (build-vector x (lambda (i) (random 1000000)))))
                (newline)
                (display-times (+ x 500)))]))

(display-times 500) 
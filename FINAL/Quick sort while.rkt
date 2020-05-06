;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Quick sort while|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

;; QUICK SORT

; qs-in-place!: (vectorof number) -> (void)
; Purpose: To sort the array in non-decreasing order.
; Effect: The elements of the array are rearranged in place.
(define (qs-in-place! V)
  (local [; swap: natnum natnum -> (void)
          ; Purpose: To swap V[i] and V[j]
          ; Effect: Modify V[i] to contain the value of V[j] and vice versa
          (define (swap i j)
            (local [(define temp (vector-ref V i))]
              (begin
                (vector-set! V i (vector-ref V j))
                (vector-set! V j temp))))

          ; partition!: VINTV(natnum natnum) natnum -> number
          ; Purpose: For the given VINTV, partition E& place pivot in final position.
          ; Effect: Mutate V so that all elements before the pivot are
          ; <= pivot and all elements after the pivot are > pivot.
          (define (partition! low high pp)
            (local
              [; small-index: VINTV(natnum natnum) natnum -> natnum
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
          
          (define i (void))
          (define j (void))
          ]
    (begin
      (set! i 0)
      (set! j (sub1 (vector-length V)))
      (while (< i j)
             (local [(define pp (partition! i j i))]
               (begin
                 (set! j (sub1 pp))
                 )))
      (set! i 0)
      (set! j (sub1 (vector-length V)))
      (while (< i j)
             (local [(define pp (partition! i j i))]
               (begin
                 (set! i (add1 pp))
                 ))))))

(define V2 (vector 10 34 3 8 27 14 9 31 7 87))

(check-expect (begin
                (qs-in-place! V2)
                V2)
              (vector 3 7 8 9 10 14 27 31 34 87))

(define (display-times x)
  (cond [(> x 15000) (void)]
        [else (begin
                (display (format "Quick Sort ~a" x))
                (newline)
                (time (qs-in-place! (build-vector x (lambda (i) (random 1000000)))))
                (newline)
                (display-times (+ x 500)))]))

(display-times 500) 
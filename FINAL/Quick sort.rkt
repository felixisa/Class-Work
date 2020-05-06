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
                          (begin
                            (qs-aux! low (sub1 pp))
                            (qs-aux! (add1 pp) high)))]))
          ]
    (qs-aux! 0 (sub1 (vector-length V)))))

(define V2 (vector 10 34 3 8 27 14 9 31 7 87))

(check-expect (begin
                (qs-in-place! V2)
                V2)
              (vector 3 7 8 9 10 14 27 31 34 87))

;; TIMING

(define (display-times x)
  (cond [(> x 15000) (void)]
        [else (begin
                (display (format "Quick Sort ~a" x))
                (newline)
                (time (qs-in-place! (build-vector x (lambda (i) (random 1000000)))))
                (newline)
                (display-times (+ x 500)))]))

(display-times 500) 
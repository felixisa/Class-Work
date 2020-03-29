;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |O'Leary-Felix-Garcia-Rachid Vector Quiz|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Grade: A

; Good job! Make sure, however, to follow all steps of
; the design recipe. See comments in your code.

(define V1 (vector 1 9 7 6 3 4 2 10 9 6))
(define (EMPTY-VINT? low high)(> low high))

; 1) Write a function that take as input a vector and
; that return the largest element of the vector

 ; largest-elem: vector -> num
 ; Purpose: Return the largest element of the vector
(define (largest-elem V)
  (local [
           ; find-largest: num num num -> num
          ; MISSING PURPOSE STATEMENT 
          (define (find-largest low high high-elem)
            (cond [(EMPTY-VINT? low high) high-elem]
                  [(> high-elem (vector-ref V low))
                   (find-largest (add1 low) high high-elem)]
                  [else (find-largest (add1 low) high (vector-ref V low))]))
          ]
    (find-largest 0 (sub1 (vector-length V)) (vector-ref V 0))))

(check-expect (largest-elem (vector 1 5 3 2 8 9 3 12 7 6)) 12)
(check-expect (largest-elem  V1) 10)

; 2) Write a function that take as input a vector and
; that return the smallest element of the vector

 ; smallest-elem: vector -> num
 ; Purpose: Return the smallest element of the vector
(define (smallest-elem V)
  (local [
          ; find-smallest: num num num -> num
          ; MISSING PURPOSE STATEMENT 
          (define (find-smallest low high low-elem)
            (cond [(EMPTY-VINT? low high) low-elem]
                  [(< low-elem (vector-ref V low))
                   (find-smallest (add1 low) high low-elem)]
                  [else (find-smallest (add1 low) high (vector-ref V low))]))
          ]
    (find-smallest 0 (sub1 (vector-length V)) (vector-ref V 0)))) 
  
(check-expect (smallest-elem (vector 0 1 5 3 2 8 9 3 12 7 6)) 0)
(check-expect (smallest-elem  V1) 1)

; 3) Write a function that takes as input a vector and
; that returns the number of elements that are a multiple of 10

 ; mults-of-10: (vector num) -> num
 ; Purpose: Determine the number of elements in the vector that are a multiple of 10
(define (mults-of-10 V)
 (local [
         ; mult-of-10?: num num -> num
         ; MISSING PURPOSE
         (define (mult-of-10? low high)
           (cond [(EMPTY-VINT? low high) 0]
                 [(= (remainder (vector-ref V low) 10) 0)
                  (+ 1 (mult-of-10? (add1 low) high))]
                 [else (mult-of-10? (add1 low) high)]))
         ]
   (mult-of-10? 0 (sub1 (vector-length V)))))

(check-expect (mults-of-10 (vector 10 15 20 30 50))4)
(check-expect (mults-of-10 V1) 1)
(check-expect (mults-of-10 (vector )) 0)

; 4)Write a function that takes as input a vector and a symbol that returns
; true if the given symbol is in the vector and false otherwise

 ; symbol-in-vector?: (vectorof symbols) symbol -> boolean
 ; Purpose: Determine if the given symbol is in the given vector
(define (symbol-in-vector? V a-sym)
  (local [
          ; in-vec?: a-num a-num -> boolean
          ; MISSING PURPOSE 
          (define (in-vec? low high)
            (cond [(EMPTY-VINT? low high) #f]
                  [(equal? (vector-ref V low) a-sym)#t]
                  [else (in-vec? (add1 low) high)]))
          ]
    (in-vec? 0 (sub1 (vector-length V)))))

(check-expect (symbol-in-vector? (vector 'hello 'goodbye 'computers) 'afternoon)#f)
(check-expect (symbol-in-vector? (vector 'hello 'goodbye 'computers) 'computers)#t)

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname OLeary-Felix-Garcia-Rachid-valid-sol) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Grade: A
; Good job! See comment about testing.

(require 2htdp/abstraction)

; Data Def
;  A board position, bpos, is a structure, (make-posn a b), where a and be are natnums

; Examples

(define P1 (make-posn 3 2))

(define P2 (make-posn 0 0))


; natnum --> (listof bpos) or false
; Purpose: To place n queens on a nxn board; otherwise return false
(define (place-queens n)
  (local (; natnum --> (listof bpos)
          ; Purpose: To returns a list of all the positions in a nxn board
          (define (total-board n)
            (for*/list [(i n) (j n)] (make-posn i j)))

          (define init-board (total-board n))

          ; bpos (listof bpos) --> (listof bpos)
          ; Purpose: To remove the positions in the given list that are
          ;          threatened by the given bpos.
          (define (rm-threatened a-bpos lob)
            (local (; bpos --> boolean
                    ; Purpose: To determine if the given bpos is threatened by a-bpos
                    ; Note: A bpos, target, is threatened by a bpos B if target and B
                    ;       are in the same column, in the same row, or if the difference
                    ;       between the xs is equal to the difference between the ys
                    ;       (i.e., target is on a diagonal from B).
                    (define (threatened? target)
                      (or (= (posn-x target) (posn-x a-bpos))
                          (= (posn-y target) (posn-y a-bpos))
                          (= (abs (- (posn-x target) (posn-x a-bpos)))
                             (abs (- (posn-y target) (posn-y a-bpos)))))))
              (filter (lambda (p) (not (threatened? p))) lob)))
          
          ; (listof bpos) natnum --> (listof bpos) or false
          ; Purpose: To place n queens in the given list of unthreatened positions
          ; Algorithm: If there are no unthreatened positions are left and there are
          ;            queens left to place, then there is no solution and return
          ;            false. Otherwise if there are no more queens to place, return
          ;            the empty list of bpos. Otherwise, try placing a queen in the
          ;            first unthreatened position and n-1 queens in the remaining
          ;            unthreatened positions. If it works, return the list with the
          ;            first unthreatened position and the solution for the n-1
          ;            queens else backtrack and try finding a solution using the
          ;            rest of unthreatened solutions.
          ;
          ; Termination: At each step, either or both the number of queens is
          ;              reduced by 1 and the remaining number of unthreatened 
          ;              positions is reduced. Eventually, either the number of
          ;              queens becomes 0 or the unthreatened positions  becomes
          ;              empty and the program terminates
          (define (place-them valid-positions n)
            (cond [(and (null? valid-positions) (> n 0)) #false]
                  [(= n 0) '()]
                  [else
                   (local ((define sol-other-queens (place-them (rm-threatened (first valid-positions)
                                                                               (rest valid-positions))
                                                                (sub1 n))))
                     (cond [(false? sol-other-queens) (place-them (rest valid-positions) n)]
                           [else (cons (first valid-positions) sol-other-queens)]))])))
    (place-them init-board n)))


 ;; valid-sol?: (listof bpos) -> boolean
 ;; Purpose: Determine if the current board is valid.
(define (valid-sol? b)
  (local
    [
     ;; in-danger?: a-bpos a-bpos -> boolean
     (define (in-danger? p1 p2)
       (or (= (posn-x p1) (posn-x p2))
           (= (posn-y p1) (posn-y p2))
           (= (abs (- (posn-x p1) (posn-x p2)))
              (abs (- (posn-y p1) (posn-y p2))))))
     
     ;; threatened?: a-bpos (listof a-bpos) -> boolean
     (define (threatened? a-bpos b)
       (ormap (lambda (x) (in-danger? x a-bpos)) b))
     ]
    (cond
      [(empty? b) #t]
      [else (and
             (not (threatened? (first b) (rest b)))
             (valid-sol? (rest b)))])))

; You should have tests with a (listof bpos) that is not a solution
; for boards of size 3 and 4.
(check-expect (valid-sol? (place-queens 0)) #true)
(check-expect (valid-sol? (place-queens 1)) #true)
(check-expect (place-queens 2) false)
(check-expect (place-queens 3) false)
(check-expect (valid-sol? (place-queens 4)) #true)
(check-expect (valid-sol? (place-queens 5)) #true)
(check-expect (valid-sol? (place-queens 8)) #true)


;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Rachid-Garcia-O'Leary-Felix-MIDTERM|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

; Grade: A-

; See comments in your code.

; Intro II Midterm
; N-PUZZLE modified by
; Isabella Felix, Jasiel Garcia, Eden O'Leary, and Mohamed Rachid 

; N-PUZZLE BASE CODE CSAS-1115 Version 1.0
; Copyright (C) 2015 by Marco T. Morazan
; Written by: Marco T. Morazan, 2015

(define N 9) ; size of the board--must be a square

(define SQLEN 100) ; the length of the side of a square in N-puzzle

(define WIDTH (* (sqrt N) SQLEN))

(define HEIGHT (* (sqrt N) SQLEN))

(define e-scene (empty-scene WIDTH (+ HEIGHT SQLEN)))

(define INITMOVES 100) ; number of moves to create an initial board


; DATA DEFINITION FOR A BOARD
; A board is a (listof natnum)

; A world is a board

; F-ON-BOARD TEMPLATE
; (define (f-on-board/world a-board)
  ; (cond [(empty? a-board) ...]
    ;     [else ...(car a-board)...(f-on-board/world (rest a-board))]))

(define WIN (build-list N (lambda (n) 
                            (cond [(< n (- N 1)) (+ n 1)]
                                  [else 0]))))

(define (top-l-corner? p) (= p 0))

(define (top-r-corner? p) (= p (- (sqrt N) 1)))

(define (bottom-l-corner? p) (= p (- N (sqrt N))))

(define (bottom-r-corner? p) (= p (- N 1)))

(define (in-top-row? p) (< p (sqrt N)))

(define (in-bottom-row? p) (>= p (- N (sqrt N))))

(define (in-left-col? p) (= (remainder p (sqrt N)) 0))

(define (in-right-col? p) (= (remainder p (sqrt N)) (- (sqrt N) 1)))

(define (get-blank-sq-num l)
  (cond [(empty? l) (error 'get-blank-sq-num "Blank not found")]
        [(= (car l) 0) 0]
        [else (add1 (get-blank-sq-num (cdr l)))]))


; make-init-world: natnum world --> world
; Purpose: To create the initial world by making the given number of moves in the given world
(define (make-init-world nummoves w)
  (cond [(= nummoves 0) w]
        [else (make-init-world (sub1 nummoves) (make-move w))]))

; make-move: world --> world
; Purpose: To make a random move in the given world
(define (make-move w)
  (local [(define blank-index (get-blank-sq-num w))
          (define bneighs (blank-neighs blank-index))
          (define move-index (list-ref bneighs (random (length bneighs))))
          ]
    (swap-tiles w move-index blank-index)))

; swap-tiles: world natnum natnum --> world
; Purpose: To swap the given tiles in the given world
(define (swap-tiles w i j)
  (build-list N (lambda (n)
                  (cond [(= n i) (list-ref w j)]
                        [(= n j) (list-ref w i)]
                        [else (list-ref w n)]))))

; blank-neighs: number --> (listof number)
; Purpose: To return a list of the tile numbers that neigbor the given blank tile number
(define (blank-neighs p)
  (cond [(top-l-corner? p)
         (list (+ p 1) (+ p (sqrt N)))]
        [(top-r-corner? p)
         (list (- p 1) (+ p (sqrt N)))]
        [(bottom-l-corner? p)
         (list (- p (sqrt N)) (+ p 1))]
        [(bottom-r-corner? p)
         (list (- p (sqrt N)) (- p 1))]
        [(in-top-row? p)
         (list (- p 1) (+ p 1) (+ p (sqrt N)))]
        [(in-bottom-row? p) 
         (list (- p (sqrt N)) (- p 1) (+ p 1))]
        [(in-left-col? p) 
         (list (- p (sqrt N)) (+ p 1) (+ p (sqrt N)))]
        [(in-right-col? p) 
         (list (- p (sqrt N)) (- p 1) (+ p (sqrt N)))]
        [else 
         (list (- p (sqrt N)) (- p 1) (+ p 1) (+ p (sqrt N)))]))

(define INIT-WORLD (make-init-world INITMOVES WIN))


;; centers of the squares in the N-puzzle

(define (compute-centers i)
  (cond [(= i 0) '()]
        [else (cons (make-posn (+ (* (remainder (- i 1) (sqrt N)) SQLEN) 50)
                               (+ (* (quotient (- i 1) (sqrt N)) SQLEN) 50))
                    (compute-centers (- i 1)))]))

(define CENTERS (reverse (compute-centers N))) ; the centers of the squares

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make-square: number --> image
; Purpose: To create a square with the given number in it
(define (make-square n)
  (cond [(= n 0) (square SQLEN "solid" "light turquoise")]
        [else (overlay/align "middle" 
                             "middle" 
                             (text/font (number->string n) 32 "black" #f "script" "italic" "normal" #f) 
                             (rectangle SQLEN SQLEN "solid" "light turquoise"))]))

(define (add-help-button scn)
  (local [(define help-button (overlay/align "middle"
                                             "middle"
                                             (text/font "Help Me Please" 20 "black" #f "script" "italic" "normal" #f)
                                             (overlay/align "middle"
                                                            "middle"
                                                            (rectangle (- (* SQLEN (sqrt N)) (/ SQLEN 2))
                                                                       (/ SQLEN 1.5)
                                                                       "solid" 
                                                                       "light purple")
                                                            (rectangle (* SQLEN (sqrt N)) SQLEN "solid" "dark purple"))))]
    (place-image help-button
                 (/ WIDTH 2)
                 (+ HEIGHT (/ SQLEN 2))
                 scn)))


; draw-world: world --> scene
; Purpose: To draw the given world in the empty-scene
(define (draw-world a-world)
  (local ((define (helper i w ctrs) 
            (cond [(empty? w) e-scene]
                  [else (place-image (make-square (car w))
                                     (posn-x (car ctrs))
                                     (posn-y (car ctrs))
                                     (helper (+ i 1) (rest w) (rest ctrs)))]))
          (define (add-h-lines scn i)
            (cond [(= i (sqrt N)) scn]
                  [else (add-h-lines (add-line scn 0 (* i SQLEN) (* (sqrt N) SQLEN) (* i SQLEN) "black") (+ i 1))]))
          (define (add-v-lines scn i)
            (cond [(= i (sqrt N)) scn]
                  [else (add-v-lines (add-line scn (* i SQLEN) 0 (* i SQLEN) (* (sqrt N) SQLEN) "black") (+ i 1))])))
    (add-help-button (add-v-lines (add-h-lines (helper 0 a-world CENTERS) 1) 1))))

;;; mouse clicking processing

; differences: board board --> (listof number)
; Purpose: To list the positions that have different tiles in two given boards
(define (differences b1 b2)
  (local [(define (helper i)
            (cond [(= i 0) empty]
                  [(= (list-ref b1 (sub1 i)) (list-ref b2 (sub1 i))) (helper (sub1 i))]
                  [else (cons (sub1 i) (helper (sub1 i)))]))]
    (helper N)))

; mouse-over-help?: number number --> boolean
; Purpose: To determine if the given coordinates are over the help button
(define (mouse-over-help? x y) (> y HEIGHT))

; process-mouse-event: world integer integer string --> world
(define (process-mouse-event w x y me)
  (cond [(string=? me "button-down") 
         (cond [(mouse-over-help? x y) 
                (local [(define solution (find-solution w))
                        (define diffs (cond [(empty? solution) empty]
                                            [else (differences w (first (rest solution)))]))]
                  (cond [(empty? diffs) w]
                        [else (swap-tiles w (first diffs) (first (rest diffs)))]))]
               [else (move-blank w (make-posn x y))])]
        [else w]))

; move-blank: world posn --> world
(define (move-blank w mpos)
  (local [(define blnk-index (get-blank-sq-num w))
          (define mouse-index (get-mouse-sq-num CENTERS mpos))]
    (cond [(not (neighs? blnk-index mouse-index)) w]
          [else (swap-blank-and-mouse w mouse-index)])))

; swap-blank-and-mouse: world number number --> world
; Purpose: To swap the mouse and blank squares in the world
(define (swap-blank-and-mouse w mouse-index)
  (local [(define mouse-val (list-ref w mouse-index))]
    (map (lambda (n)
           (cond [(= n 0) mouse-val]
                 [(= n mouse-val) 0]
                 [else n]))
         w)))

; mouse-in-square?: posn posn --> boolean
; Purpose: To determine if the first posn is in the square that has the 2nd posn as its center
(define (mouse-in-square? mposn scenter)
  (and (< (abs (- (posn-x mposn) (posn-x scenter))) (/ SQLEN 2))
       (< (abs (- (posn-y mposn) (posn-y scenter))) (/ SQLEN 2))))

; get-mouse-sq-num: world posn --> number
; Purpose: Return the position in the given world of the square the mouse is over or
;          -1 if the mouse is not over a square
(define (get-mouse-sq-num centers mposn)
  (local [(define (helper centers i)
            (cond [(empty? centers) -1]
                  [(mouse-in-square? mposn (first centers)) i]
                  [else (helper (rest centers) (add1 i))]))]
    (helper centers 0)))

; neighs?: number number --> boolean
; Purpose: To determine if the given mouse index and blank indes are neighbors
(define (neighs? bindex mindex)
  (cond [(= mindex -1) false]
        [(top-l-corner? bindex)
         (or (= mindex (+ bindex 1))
             (= mindex (+ bindex (sqrt N))))]
        [(top-r-corner? bindex)
         (or (= mindex (- bindex 1))
             (= mindex (+ bindex (sqrt N))))]
        [(bottom-l-corner? bindex)
         (or (= mindex (- bindex (sqrt N)))
             (= mindex (+ bindex 1)))]
        [(bottom-r-corner? bindex)
         (or (= mindex (- bindex (sqrt N)))
             (= mindex (- bindex 1)))]
        [(in-top-row? bindex)
         (or (= mindex (- bindex 1))
             (= mindex (+ bindex 1))
             (= mindex (+ bindex (sqrt N))))]
        [(in-bottom-row? bindex) 
         (or (= mindex (- bindex (sqrt N)))
             (= mindex (- bindex 1))
             (= mindex (+ bindex 1)))]
        [(in-left-col? bindex) 
         (or (= mindex (- bindex (sqrt N)))
             (= mindex (+ bindex 1))
             (= mindex (+ bindex (sqrt N))))]
        [(in-right-col? bindex) 
         (or (= mindex (- bindex (sqrt N)))
             (= mindex (- bindex 1))
             (= mindex (+ bindex (sqrt N))))]
        [else 
         (or (= mindex (- bindex (sqrt N)))
             (= mindex (- bindex 1))
             (= mindex (+ bindex 1))
             (= mindex (+ bindex (sqrt N))))]))

; mouse-on-neigh-of-blank?: (listof posn) posn --> boolean
(define (mouse-on-neigh-of-blank? bneighs-posns mposn)
  (cond [(empty? bneighs-posns) false]
        [(mouse-in-square? mposn (car bneighs-posns)) true]
        [else (mouse-on-neigh-of-blank? (rest bneighs-posns) mposn)]))

; win?: world --> boolean
; Purpose: To determine if the given world is WIN
(define (win? w) (equal? w WIN))

(define (make-win-scene w)    
  (place-image (text/font "Did you just use\nthe help button?" 30 "turquoise" #f "script" "italic" "bold" #f)
               (/ WIDTH 2)
               (/ HEIGHT 2)
               (draw-world w)))

; generate-children: board → non-empty-list-of-boards
; Purpose: To generate a list of the children of the given board
(define (generate-children b)
  (local [(define blank-pos (get-blank-sq-num b))]
    (map (lambda (p) (swap-tiles b blank-pos p))
         (blank-neighs blank-pos))))

; manhattan-distance: board → number
; Purpose: To compute the Manhattan distance of the given board
(define (manhattan-distance b)
  (local
    [; distance: number number --> number
     ; Purpose: To compute the distance between the two tile positions
     (define (distance curr corr)
       (+ (abs (- (quotient curr (sqrt N)) (quotient corr (sqrt N))))
          (abs (- (remainder curr (sqrt N)) (remainder corr (sqrt N))))))
     ; correct-pos: number --> number
     ; Purpose: To determine the correct position of the given tile
     (define (correct-pos n)
       (cond [(= n 0) (sub1 N)]
             [else (sub1 n)]))
     ; adder: number --> number
     ; Purpose: To add all the distances of each tile
     (define (adder pos)
       (cond [(= pos 0) 0]
             [else (+ (distance (sub1 pos) (correct-pos (list-ref b (sub1 pos))))
                      (adder (sub1 pos)))]))]
    (adder N)))

; DATA DEFINITION

; A sequence is either
  ; 1. (list world)
  ; 2. (cons w s), where w is a world and s is a sequence.

; A list of sequence (lseq) is either
  ; 1. empty
  ; 2. (cons s l), where s is a sequence and l is a lseq


; find-solution: board --> lseq
; Purpose: To find a solution to the given board
; TERMINATION ARGUMENT: find-solution takes as input a single board and proceeds to create paths by creating the children
; of that board by exploring each possible move of the zero space. Then, the "best-path", or that whose first element has the smallest 
; manhattan distance, is selected as the most promising sequence and thus is explored first, avoiding uneccesary exploration of other boards.
; The children of this board are then added to the paths and the process of finding the most promising path is recursed, accumulating paths.
; On each recursive call, paths whose first element

; This is incorrect. It is the visited children of the first board of the best path that are ignored.

; has been visited are ignored, avoiding the exploration of already explored boards.
; This also ensures an always finite number of boards, and as a a result, a finite number of paths that are explored.
; The function terminates when the best-path has eventually generated a child that is equal to the winning board. 
(define (find-solution b)
  (local [
          ; search-paths: lseq --> seq
          ; Purpose: To find a solution to b by exploring the most promising sequence first, ignoring visited successors
          ; ACCUMULATOR INVARIANTS:
          ; paths is a list of all sequences starting at b, the board we are solving, generated so far that have no repeated nodes.
          ; Paths does not included the nodes that have been visited already, which are filtered out.
          ; visited is the list of boards encountered on all sequences explored so far. If this current board is not a winning board,
          ; visited updates by consing the current board to the list of boards already checked, or consing the current board to visited.
          (define (search-paths paths visited)
            (local [
                    ; best-path-accum: (listof (listof board)) -> (listof (listof board))
                    ; Purpose: To find the most promising path (the path with the smallest manhattan distance) in a list of paths
                    ; ACCUM INVARIANT: accum is the path whose first element has the smallest manhattan distance found in the list of paths so far.

                    ; This function is more complex than should be, because you never use (first lop). 
                    
                    (define (best-path-accum accum lop)
                      (cond [(null? (cdr lop)) accum]
                            [(> (manhattan-distance (car accum))
                                (manhattan-distance (caar (rest lop))))
                             (best-path-accum (car (rest lop))
                                              (cdr lop))]
                            [else (best-path-accum accum (cdr lop))]))

                    (define best-path (best-path-accum (car paths)
                                                       paths
                                                       ; Why keep the first?
                                                       )) 
                    (define children (filter (lambda (x) (not (member x visited)))
                                             (generate-children (car best-path))))
                    (define new-paths (map (lambda (c) (cons c best-path))
                                           children))
                    ]
              (cond [(equal? (car best-path) WIN) best-path]
                    [else (search-paths
                           (append (filter (lambda (x) (not (member x visited)))
                                           ; x is a path. visited is a list of boards. Why are you trying to filter a path
                                           ; from a list of boards? Nothing ever gets removed and can only make the game slow.
                                           (remove best-path paths))
                                   new-paths)
                           (cons (car best-path) visited))])))
          ]
    (reverse (search-paths (list (list b)) '()))))

(check-expect (find-solution (list 1 2 3 4 5 6 7 8 0)) (list WIN))
(check-expect (find-solution (list 1 3 6 4 5 2 7 8 0))
              (list
               (list 1 3 6 4 5 2 7 8 0)
               (list 1 3 6 4 5 2 7 0 8)
               (list 1 3 6 4 0 2 7 5 8)
               (list 1 3 6 4 2 0 7 5 8)
               (list 1 3 0 4 2 6 7 5 8)
               (list 1 0 3 4 2 6 7 5 8)
               (list 1 2 3 4 0 6 7 5 8)
               (list 1 2 3 4 5 6 7 0 8)
               (list 1 2 3 4 5 6 7 8 0)))

(define (run-game nm) 
  (big-bang 
      (make-init-world INITMOVES WIN)
    (name nm)
    (on-draw draw-world)
    (on-mouse process-mouse-event)
    (stop-when win? make-win-scene)))
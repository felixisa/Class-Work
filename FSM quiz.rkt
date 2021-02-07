;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |FSM quiz|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; Isabella Felix, Mohamed Rachid

; Grade: A

; Your design is almost perfect. My only concern
; is your use of a conditional. See the comment in
; your code.


;; Data Definitions ;;
; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
(define-struct fsm [initial transitions final])
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
(define-struct transition [current key next])
; An FSM-State is String.

(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

(define fsm-contains-abc
  (make-fsm
   "Q0"
   (list (make-transition "Q0" "a" "Q1")
         (make-transition "Q0" "b" "Q0")
         (make-transition "Q0" "c" "Q0") 
         (make-transition "Q1" "a" "Q1")
         (make-transition "Q1" "c" "Q0")
         (make-transition "Q1" "b" "Q2")
         (make-transition "Q2" "a" "Q0")
         (make-transition "Q2" "b" "Q0")
         (make-transition "Q2" "c" "Q3")
         (make-transition "Q3" "a" "Q3")
         (make-transition "Q3" "b" "Q3")
         (make-transition "Q3" "c" "Q3"))
   "Q3"))

(define fsm-even-num-b
  (make-fsm
   "Q0"
   (list (make-transition "Q0" "a" "Q0")
         (make-transition "Q0" "b" "Q1")
         (make-transition "Q1" "a" "Q1")
         (make-transition "Q1" "b" "Q0"))
   "Q0"))

;; Functions ;;

; fsm-match?: FSM string -> boolean
; Purpose: Determine if the input is accepted by the FSM
(define (fsm-match? a-fsm k)
  (local [; constants 
          (define initial (fsm-initial a-fsm))
          (define transitions (fsm-transitions a-fsm)) 
          (define final (fsm-final a-fsm))
          (define str (explode k))

          ; next-state: state string transition -> state
          ; Purpose: To determine the next state
          
          ; Why do you have a conditional here?
          ; Is your and expression always true?
          (define (next-state st k trans)
            ;(cond [(and (string=? st (transition-current trans))
             ;           (string=? k (transition-key trans)))
                   (transition-next trans)
            ;])
            )

          ; find-trans: FSM string -> (listof transitions) 
          ; Purpose: Finds the appropriate transition if it exists 
          (define (find-trans a-fsm k)
            (filter (lambda (x) (and (string=? initial (transition-current x))
                                     (string=? (first k) (transition-key x))))
                    transitions))]
    
    (cond [(and (empty? str)
                (string=? initial final)) #t]
          [(or (and (empty? str)
                    (not (string=? initial final)))
               (empty? (find-trans a-fsm str))) #f]
          [else (fsm-match? (make-fsm (next-state initial
                                                  (first str)
                                                  (first (find-trans a-fsm str)))
                                      transitions
                                      final)
                            (substring k 1 (string-length k)))])))

(check-expect (fsm-match? fsm-a-bc*-d "abcd") #t)
(check-expect (fsm-match? fsm-a-bc*-d "ad") #t)
(check-expect (fsm-match? fsm-a-bc*-d "bad") #f)
(check-expect (fsm-match? fsm-a-bc*-d "ff") #f)

(check-expect (fsm-match? fsm-even-num-b "abab") #t)
(check-expect (fsm-match? fsm-even-num-b "bb") #t)
(check-expect (fsm-match? fsm-even-num-b "aba") #f)
(check-expect (fsm-match? fsm-even-num-b "ccc") #f)

(check-expect (fsm-match? fsm-contains-abc "abc")#t)
(check-expect (fsm-match? fsm-contains-abc "aabcc")#t)
(check-expect (fsm-match? fsm-contains-abc "") #f)
(check-expect (fsm-match? fsm-contains-abc "aba")#f)
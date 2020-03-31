;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Felix-Gracia-OLeary-Rachid-checking-accounts) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; An account is an interface
; 1. 'deposit: number -> (void)
; 2. 'withdraw: number -> (void) 
; 3. 'balance: -> number
; 4. 'owner: -> symbol 

; owner is a symbol
; balance is a number 
(define-struct account (owner balance)) 

(define (make-check-acc n)
  (local [; check-acc: structure
          ; Purpose: To maintain a structure containing owner name and balance
          (define my-check-acc (make-account n 0))

          ; deposit: number -> (void)
          ; Purpose: To add an amount to the balance
          ; Effect: Increases account balance by given amount 
          (define (deposit a)
            (local [(define u (if (negative? a)
                                  (error "Cannot Deposit Negative Amount")
                                  (set-account-balance! my-check-acc (+ a (account-balance my-check-acc)))))]
              (void)))
          
          ; withdraw: number -> (void) 
          ; Purpose: To subtract an amount from the balance 
          ; Effect: Decreases account balance by given amount
          (define (withdraw a)
            (local [(define u (if (> a (account-balance my-check-acc))
                                  (error (format "Insufficient Balance to Withdraw ~s" a))
                                  (set-account-balance! my-check-acc (- (account-balance my-check-acc) a))))]
              (void))) 
          
          (define (account-manager msg)
            (cond [(eq? msg 'deposit) deposit]
                  [(eq? msg 'withdraw) withdraw]
                  [(eq? msg 'owner) (account-owner my-check-acc)]
                  [(eq? msg 'balance) (account-balance my-check-acc)]
                  [else (error 'service-manager "Unknown Service Requested: " msg)]))]
    account-manager))

; deposit: number account -> void
; Purpose: wrap function to deposit 
(define (deposit number acc)
  ((acc 'deposit) number))

(check-expect (begin
                (deposit 300 account1)
                (balance account1))
              300)

; withdraw: number account -> void
; Purpose: wrap function to withdraw 
(define (withdraw number acc)
  ((acc 'withdraw) number))

(check-expect (begin
                (withdraw 300 account1)
                (balance account1))
              0)

; owner: account -> symbol
; Purpose: wrap function to inquire owner 
(define (owner acc)
  (acc 'owner))

(check-expect (owner account2) 'Mohamed)

; balance: account -> number
; Purpose: wrap function to inquire balance 
(define (balance acc)
  (acc 'balance))

(check-expect (begin
                (deposit 0 account1)
                (balance account1))
              0)

(define account1 (make-check-acc 'Isabella))
(define account2 (make-check-acc 'Mohamed))
(define account3 (make-check-acc 'Jasiel))
(define account4 (make-check-acc 'Eden))
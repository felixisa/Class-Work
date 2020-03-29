;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Felix-Garcia-OLeary-Rachid-triangulate) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; Grade: A+
; Great Job!!


;; Data Definitions

;; An SOE is a non-empty Matrix.
;; Constraint: for (list r1 ... rn), (length ri) is (+ n 1)
;; Interpretation: represents a system of linear equations
 
;; An Equation is a [List-of Number].
;; Constraint: an Equation contains at least two numbers. 
;; Interpretation: if (list a1 ... an b) is an Equation, 
;; a1, ..., an are the left-hand-side variable coefficients and b is the right-hand side
 
;; A Solution is a [List-of Number]

(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))
(define M2
  (list (list 2 1 3 1)
        (list 2 6 8 3)
        (list 6 8 18 5)))
(define M3
  (list (list 19 6 2 27)
        (list 15 8 10 30)
        (list 5 10 20 60)))

(define S (list 1 1 2)) ; a Solution
(define S2 (list .3 .4 0))
(define S3 (list -9 37.5 -13.5))
;; A Triangulated Matrix (TM) is a [NEList-of Equation] such that the Equations are of decreasing length: 
;; n + 1, n, n - 1, ..., 2. 
;; Interpretation: represents a triangular matrix
(define TM ; a triangulated matrix
  (list (list 2 2 3 10)
        (list   3 9 21)
        (list     1  2)))
(define TM2
  (list (list 2 1 3 1)
        (list 5 5 2)
        (list 4 0)))

;; Functions

;; lhs: Equation -> [List-of Number]
;; Purpose: extracts the left-hand side from a row in a matrix
(define (lhs e) (reverse (rest (reverse e))))

(check-expect (lhs (first M)) (list 2 2 3))

;; rhs: Equation -> Number
;; Purpose: extracts the right-hand side from a row in a matrix
(define (rhs e)(first (reverse e)))

(check-expect (rhs (first M)) 10)

;; plug-in: (left-side-of-equation) Solution -> number
;; Purpose: Plug in the solution into the variables of the equation
(define (plug-in left-side solution)
  (cond [(empty? left-side) 0]
        [else (+ (* (first left-side) (first solution))
                 (plug-in (rest left-side) (rest solution)))]))

(check-expect (plug-in (lhs (first M)) S) 10)
(check-expect (plug-in empty S) 0)

;; check-solution: SOE Solution -> boolean
;; Purpose: To determine whether the given solution is true for the given SOE
(define (check-solution SOE sol)
  (andmap (lambda (x) (= (plug-in (lhs x) sol) (rhs x))) SOE))

(check-expect (check-solution M (list 1 1 2)) #t)
(check-expect (check-solution M (list 2 1 3)) #f)

;; triangulate: SOE -> TM
;; Purpose: Triangulates the given system of equations
;; Assume: The input equations are of equal length
(define (triangulate m)
  (cond [(<= (length (first (reverse m))) 2) m]
        [(andmap (lambda (x) (= 0 (first x))) m) (error "leading coefficients zero")]
        [(= (first (first m)) 0) (triangulate (append (rest m) (list (first m))))]
        [else (local [(define firstM (first m))
                      ;; subtract: eq eq -> eq
                      ;; Purpose: Subtract the first equation from the second one
                      (define (subtract eq1 eq2)
                        (local[(define scaler (/ (first eq2) (first eq1)))]
                          (map (lambda (x y) (- y (* scaler x))) (rest eq1) (rest eq2))))

                      ;; sub-by-first: eq matrix -> matrix
                      ;; Purpose: Subtract (rest matrix) by the first of the matrix
                      (define (sub-by-first eq M) (map (lambda (x) (subtract eq x)) (rest M)))] 
                (cons firstM (triangulate (sub-by-first firstM m))))]))

(check-expect (triangulate M) TM)
(check-expect (triangulate M2) TM2)

;(check-expect (triangulate (list (list 0 2 3 4)
;                                 (list 0 3 4 5)
;                                 (list 0 5 6 7))) (error "leading coefficients zero"))
(check-expect (triangulate (list (list 0 2 3 4)
                                 (list 5 6 7 8)
                                 (list 9 10 11 12))) (triangulate (list (list 5 6 7 8)
                                                                        (list 9 10 11 12)
                                                                        (list 0 2 3 4))))

;; solve-le: equation solution -> solution
;; Purpose: Solve an equation knowing all but one answer of the solutions
(define (solve-le le sol)
  (cons (/ (- (rhs le) (plug-in (rest (lhs le)) sol))
           (first le))
        sol))

(check-expect (solve-le (list 3 9 21) (list 2)) (list 1 2))

;; solve: TM -> Solution
;; Purpose: Find the solution of the matrix
(define (solve TM)
  (foldr solve-le '() TM))

(check-expect (solve (triangulate M)) S)
(check-expect (solve (triangulate M2)) S2)
(check-expect (solve (triangulate M3)) S3)

;; gauss: matrix -> solution
;; Purpose: Solve a matrix using triangulation
(define (gauss m)
  (solve (triangulate m)))

(check-expect (gauss M) S)
(check-expect (gauss M2) S2)
(check-expect (gauss M3) S3)
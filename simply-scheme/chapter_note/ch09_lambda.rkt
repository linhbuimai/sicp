"""
What: Lambda (anonymous function)
"""

;; what: create anonymous function that return a proc
(define (add-three-to-each sent)
  (every 
    (lambda (number) (+ number 3)) 
    sent
  )
)

; example of using lambda in higher-order function
(define (apply-func func lst)
  (map func lst))

(apply-func (lambda (x) (+ x 3)) '(1 2 3 4))

(every (lambda (wd) (se (first wd) wd (last wd)))
  '(only a northern song)
)

(keep (lambda (n) (member? 9 n)) '(4 81 909 1969 1776))

(accumulate (lambda (this that)
              (if (> (count this) (count that)) this that))
            '(wild honey pie))

"""
Use lambda to write function that returns funtion
"""
(define (make-adder num) (lambda (x) (+ x num)))

; comment:
; when call (make-adder 4) --> return a fuction that will add 4 to some value
; ((make-adder 4) 7) <-- add 4 to 7 

;; the below procedure receive another procedure fn
(define (same-arg-twice fn) (lambda (arg) (fn arg arg)))

(define (flip fn) (lambda (a b) (fn b a)))

; example of using let
(define (roots a b c)
  (let 
    ; local variable declaration
    ( (discriminant (sqrt (- (* b b) (* 4 a c)))) )
    (se (/ (+ (- b) discriminant) (* 2 a))
        (/ (- (- b) discriminant) (* 2 a)))
  )
)
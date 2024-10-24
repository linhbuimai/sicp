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
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
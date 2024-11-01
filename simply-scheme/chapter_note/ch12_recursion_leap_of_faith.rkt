(require (planet dyoo/simply-scheme:1:2/simply-scheme))

"""
What: Understand recursion in terms of leap-of-faith method (top-down)

- step 1: write the recursive function directly with assumption that it has already worked
- step 2: write the base case
"""

(define (reverse wd)
  (if (= (count wd) 1) 
    wd
    (word (last wd) (reverse (bl wd)))
  )
)

;; example: factorial (calculate n!)

(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))
  )
)

;; example: evens

(define (evens sent)
  (if (<= (count sent) 1)
    '()
    (se (first (bf sent)) (evens (bf (bf sent))) )
  )
)
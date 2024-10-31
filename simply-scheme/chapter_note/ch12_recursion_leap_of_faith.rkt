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

;; example: factorial

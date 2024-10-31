(require (planet dyoo/simply-scheme:1:2/simply-scheme))

"""
What: Introduction to Recursion

Write recursion function using 'combining method' (bottom-up)
"""

(define (downup wd)
  (if (= (count wd) 1)
    (se wd)
    (se wd (downup (bl wd)) wd)
  )
)

;; example: Pig Latin
; move all initial consonants to the end and add 'ay'

(define (pigl wd)
  (if (member? (first wd) 'aeiou)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))
  )
)

;; Problem: (explode 'dynamite) > (d y n a m i t e)
(define (explode wd)
  (if (empty? wd)
    '()
    (se (first wd) (explode (bf wd)))
  )
)

;; Problem: (letter-pairs 'george) > (ge eo, or rg ge)
(define (letter-pairs wd)
  (if (<= (count wd) 2)
    (se wd)
    (se (word (first wd) (first (bf wd))) (letter-pairs (bf wd)))
  )
)
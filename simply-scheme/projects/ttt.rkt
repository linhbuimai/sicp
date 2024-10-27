(require (planet dyoo/simply-scheme:1:2/simply-scheme))

"""
(1) Representation of the board:
'--------- ('123456789) which might be replaced by x and o during game playing
'--o-xox-x

(2) Combination of possible wins:
'(123 456 789 147 258 369 159 357)

Our tasks is to map the representation of the board to to combination of possible wins
so that we know which step should take.
"""

(define (substitute-letter square position)
  (if (equal? '_ (item square position)) square (item square position))
)

(define (substitute-triple combination position)
  (accumulate word
    (every 
      (lambda (square) (substitute-letter square position))
      combination
    )
  )
)

(define (find-triples position)
  (every (lambda (comb) (substitute-triple comb position))
    '(123 456 789 147 258 369 159 357))
)
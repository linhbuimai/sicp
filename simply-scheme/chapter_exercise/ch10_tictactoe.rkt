"""
Shorter version of ch10_tictactoe_v02

Possible test positions:
'xo__ooxxo
'oxooxxxo_
"""

;; 10.1 Check if a player has already won
(define (helper-check-won triple letter)
  (or (my-pair? triple letter) (= (appearances letter triple) 3))
)

(define (check-won triples letter)
  (keep (lambda (triple) (helper-check-won triple letter)) triples)
)

(define (already-won? position me)
  (if (empty? (check-won (find-triples position) me)) #f #t)
)

;; 10.2, 10.3 Check if the game has ended in a tie
(define (who-can-win? position)
  (cond 
    ((already-won? position 'x) 'x)
    ((already-won? position 'o) 'o)
    (else #f)
  )
)

(define (tie-game? position)
  (and (not (who-can-win? position)) (<= (appearances '_ position) 1)))

;; TODO: check tie with board with two square?


;; 10.4 Change rule

; Rule: Win when having three squares forming an L share in a corner

; Rule: Only columns and rows win

; Rule: Win when having four squares in a corner
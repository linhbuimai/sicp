(require (planet dyoo/simply-scheme:1:2/simply-scheme))

"""
Suites:
c = clubs (tép)
s = spades (bích)
h = hearts (cơ)
d = diamonds (dô)

(a)ce = 4
(k)ing = 3
(q)ueen = 2
(j)ack = 1
others = 0

extra points for suits: (4 suits)
- double    = two cards of a particular suit = 1 point
- singleton = one card of a particular suit = 2 points
- void      = no cards in a particular suit = 3 points

cards are represented by word like: h5 (suit-rank)
a hand is a sentence of cards:

(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)
"""

"""
HOW-TO approach: bottom-up (write the most detailed, low-level proc first)
"""

; -------- Card-val -------- ;

(define (card-val card)
  (let ( (rank (butfirst card)) )
    (cond 
      ((equal? rank 'a) 4)
      ((equal? rank 'k) 3)
      ((equal? rank 'q) 2)
      ((equal? rank 'j) 1)
      (else 0)
    )
  )
)

; test
(card-val 'cq)
(card-val 's7)
(card-val 'ha)

; -------- High-card-points -------- ;

(define (high-card-points hand)
  (accumulate + (every card-val hand))
)

(high-card-points '(sa s10 hq ck c4))
(high-card-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))

; -------- Count-suit -------- ;
(define (count sent) (accumulate + (every (lambda (arg) 1) sent)))

(define (count-suit suit hand)
  (count (keep (lambda (x) (equal? suit x)) (every first hand)))
)

(count-suit 's '(sa s10 hq ck c4))
(count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))
(count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))

; -------- Suit-counts -------- ;
; (spades hearts clubs diamonds)
(define (suit-counts hand)
  (every (lambda (x) (count-suit x hand)) '(s h c d))
)

(suit-counts '(sa s10 hq ck c4))
(suit-counts '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))
(suit-counts '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))

; -------- Suit-dist-points -------- ;
(define (suit-dist-points num)
  (cond 
    ((= num 2) 1)
    ((= num 1) 2)
    ((= num 0) 3)
    (else 0)
  )
)

(suit-dist-points 2)
(suit-dist-points 7)
(suit-dist-points 0)

; -------- Hand-dist-points -------- ;

(define (hand-dist-points hand)
  (accumulate + (every suit-dist-points (suit-counts hand)))
)

(hand-dist-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))
(hand-dist-points '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))

; -------- Bridge-val -------- ;
; combine: high-card-points and hand-dist-points
(define (bridge-val hand)
  (+ (high-card-points hand) (hand-dist-points hand))
)

(bridge-val '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))
(bridge-val '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))
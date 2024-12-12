(require (planet dyoo/simply-scheme:1:2/simply-scheme))

"""
Suites:
c = clubs (tép)
s = spades (bích)
h = hearts (cơ)
d = diamonds (dô)

(a)ce
(k)ing
(q)ueen
(j)ack

Poker hands have five cards.

Kinds of poker hands (desc order of value):

1. royal flush = same suit: 10, j, q, k, a
2. straight flush = same suit: five cards of sequential rank
3. four of a kind = four cards of the same rank
4. full house = three cards of the same rank, two of a second rank
5. flush = five cards of the same suit, not sequential rank
6. straight = five cards of sequential rank, not the same suit
7. three of a kind = three cards of the same rank, no other matches
8. two pair = two pairs of cards, of two different ranks
9. pair = two cards of the same rank, no other matches
10. nothing: none of the above
"""

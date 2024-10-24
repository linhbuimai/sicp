"""
What: Higher Order Function
"""

"""
1) Using `every` to apply a function to every item in a sequence.
"""
;; explain: Apply the function `first` to each item in the `sent` variable
(define (first-letters sent)
  (every first sent))
; example
(first-letters '(here come the sun))

;; write a custom function then apply it with every
(define (plural noun)
  (if (equal? (last noun) 'y)
      (word (bl noun) 'ies)
      (word noun 's)))
; example
(every plural '(beatle turtle holly kink zombie))

(define (double letter) (word letter letter))

"""
2) Using `keep` to filter out values from a sequence.
"""
;; what: remove item does not end with letter e
(define (end-e? word) (equal? (last word) 'e))
(keep end-e? '(please put the salami above the blue elephant))

;; what: remove acronym word from a sentence
(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))
(keep real-word? '(lucy in the sky with diamonds))
; get the first letter of each real word A.K.A combine `every` with `keep`
(every first (
  keep real-word? '(lucy in the sky with diamonds)
))

"""
3) Using `accumulate` to aggregate every items in a sequence
It aggregate each two elements, receive result, the aggregate that result to the 3rd element
then so on
"""
(accumulate word '(a c l u))
(accumulate max '(6 3 4 -5 8 7))

;; what: add hyphenate to all items in a sentence
(define (hyphenate wd1 wd2)
  (word wd1 '- wd2))
(accumulate hyphenate '(ob la di ob la da))

;; Combining Higher-Order Functions
;; what: implement the 'count' procedure to count number of word in a sentence, or number of letters in a word
(define (always-one arg) 1)
(define (count sent)
  (accumulate + (every always-one sent)))

"""
4) Using `repeated` as function return another function
"""
;; what: repeate the function square 2 times for argument 3
(define (square num) (* num num))
((repeated square 2) 3)
; explain: the (repeated square 2) returns a procedure that later then invokes the square function two times

;; what: return a particular element at the nth position of a sentence
(define (item n sent)
  (first ((repeated bf (- n 1)) sent))
)

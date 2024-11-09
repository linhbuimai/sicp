"""
Project: Spelling names of huge numbers
"""

(require (planet dyoo/simply-scheme:1:2/simply-scheme))

;;; What: Spell each three digit groups ;;;

(define num-under-20 '(one two three four five six seven eight nine ten eleven twelve thirdteen fourteen fifteen sixteen seventeen eighteen nineteen))
(define num-over-20 '(twenty thirty fourty fifty sixty seventy eighty ninety))

; remove leading zero in each digit group
(define (remove-leading-zero grp)
  (cond 
    ((empty? grp) "")
    ((= (first grp) 0) (remove-leading-zero (bf grp)))
    (else grp)
  )
)

; map the number to its correct spell
(define (mapping-spell-digit-helper dgt sent starter period counter)
  (cond
    ((= counter 0) "")
    ((= dgt starter) (first sent))
    (else 
      (mapping-spell-digit-helper dgt (bf sent) (+ starter period) period (- counter 1))
    )
  )
)

(define (mapping-spell-digit-under-20 dgt)
  (mapping-spell-digit-helper dgt num-under-20 1 1 (count num-under-20))
)

(define (mapping-spell-digit-over-20 dgt)
  (mapping-spell-digit-helper dgt num-over-20 20 10 (count num-over-20))
)

; map the whole group of three digits (this function only receive the group having three digits)

(define (mapping-two-digits grp)
  (cond
    ((equal? grp "00") '())
    ((< grp 20) (mapping-spell-digit-under-20 grp))
    ((and (>= grp 20) (= (last grp) 0))
      (mapping-spell-digit-over-20 grp)
    )
    (else
      (se 
        (mapping-spell-digit-over-20 (word (first grp) "0")) 
        (mapping-spell-digit-under-20 (bf grp)))
    )
  )
)

(define (mapping-individual-group grp)
  (let
    ((grp-remove-zero (remove-leading-zero grp)))
    (cond 
      ((empty? grp-remove-zero) '())
      ((= (count grp-remove-zero) 1)
        (mapping-spell-digit-under-20 grp-remove-zero)
      )
      ((= (count grp-remove-zero) 2)
        (mapping-two-digits grp-remove-zero)
      )
      (else
        (se (mapping-spell-digit-under-20 (first grp)) 'hundred (mapping-two-digits (bf grp)))
      )
    )
  )
)

; test
(mapping-individual-group '123)
(mapping-individual-group '023)
(mapping-individual-group '3)
(mapping-individual-group '000)
(mapping-individual-group '12)
(mapping-individual-group '100)

;;; WHAT: Split the original number into group of three digits start from right to left
;;;       then map it with the correct spell

(define group-digits-text
  '(thousand million billion trillion quadrillion quintillion sextillion septillion octillion nonillion decillion)
)

;;; Split the number to group of three digits from right to left ;;;

(define (get-last-n wd n)
  (if (= n 0) "" (word (get-last-n (bl wd) (- n 1)) (last wd)))
)

(define (split-num-helper wd counter)
  (if (<= counter 3) (se wd)
    (se (get-last-n wd 3) (split-num-helper (bl (bl (bl wd))) (- counter 3)))
  )
)

(define (split-num wd)
  (split-num-helper wd (count wd))
)

(split-num '9837467)
(split-num '19372081280)

;;; Mapping the digit groups with its correct spells ;;;
(define (number-name-helper sent sent-map)
  (cond
    ((empty? sent) '())
    ((equal? (first sent) "000") (number-name-helper (bf sent) (bf sent-map)))
    (else
      (se 
        (number-name-helper (bf sent) (bf sent-map)) 
        (mapping-individual-group (first sent))       
        (first sent-map)
      )
    )
  )
)

(define (number-name num)
  (let
    ((group-digits (split-num num)))
    (se 
      (number-name-helper (bf group-digits) group-digits-text)
      (mapping-individual-group (first group-digits))
    )
  )
)

(number-name '9837467)
(number-name '19372081280)
(number-name '100038462)
(number-name '100038000)
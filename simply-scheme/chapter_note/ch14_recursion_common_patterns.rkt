"""
Common patterns in recursive procedures
"""

"every pattern"
(define (square-sent sent)
  (if (empty? sent)
    '()
    (se (square (first sent)) (square-sent (bf sent)))
  )   
)

"keep pattern"
(define (keep-three-letter-words sent)
  (cond ((empty? sent) '())
    ((= (count sent) 3)
      (se (first sent) (keep-three-letter-words (bf sent)))
    )
    (else
      (keep-three-letter-words (bf sent))
    )
  )
)

"accumulate pattern"

; example base case result is identity element of the combiner function
(define (addup nums)
  (if (empty? nums)
    0
    (+ (first nums) (addup (bf nums)))
  )
)

; example base case result is not identity element of the combiner function
(define (sent-max sent)
  (if (= (count sent) 1)
    (first sent) ; ** base case modification
    (max (first sent) (sent-max (bf sent)))
  )
)

"combining pattern = keep and accumulate"

(define (add-numbers sent)
  (cond ((empty? sent) 0)
    ((number? (first sent))
      (+ (first sent) (add-numbers (bf sent)))
    )
    (else (add-numbers (bf sent)))
  )
)

"COMBINES all three patterns every, keep, accumulate"

; the function that selects "real" words of the function, takes the first 
;   letter of each word  and combines these initial letters into a single word

(define (real-word? wd)
  (not (member? wd '(a the an in of and for  to with))))

(define (acronym sent)
  (cond
    ((empty? sent) "")
    ((real-word? (first sent))
      (word (first (fisrt sent)) (acronym (bf sent)))
    )
    (else (acronym (bf sent)))
  )
)

"When we need: Helper function"

; get every nth item in a sentence
(define (every-nth-helper interval remaining sent)
  (cond 
    ((empty? sent) '())
    ((= remaining 1)
      (se (first sent) (every-nth-helper interval interval (bf sent))))
    (else
      (every-nth-helper interval (- remaining 1) (bf sent)))
  )
)

(define (every-nth n sent)
  (every-nth-helper n n sent)
)
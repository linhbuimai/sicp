(require (planet dyoo/simply-scheme:1:2/simply-scheme))

;; ---- General requirements
; 1. classify the recursive pattern
; 2. write the recursive function

; 14.1 using keep pattern
(define (remove-once wd sent)
  (cond
    ((empty? sent) '())
    ((equal? (first sent) wd) (bf sent))
    (else (se (first sent) (remove-once wd (bf sent))))
  )
)

; 14.2 every pattern with helper function
(define (up-helper wd sent)
  (cond
    ((empty? wd) sent)
    ((empty? sent) (up-helper (bf wd) (se (first wd))))
    (else
      (up-helper (bf wd) (se sent (word (last sent) (first wd))))
    )
  )
)

(define (up wd)
  (up-helper wd '())
)

; 14.3 keep pattern with helper procedure
(define (remdup-helper sent sentResult)
  (cond
    ((empty? sent) sentResult)
    ((member? (first sent) sentResult) (remdup-helper (bf sent) sentResult))
    (else
      (remdup-helper (bf sent) (se sentResult (first sent)))
    )
  )
)

(define (remdup sent)
  (remdup-helper sent '())
)

; 14.4 keep pattern with helper procedure
(define (odds-helper sent counter)
  (cond
    ((empty? sent) '())
    ((odd? counter) (se (first sent) (odds-helper (bf sent) (+ counter 1)) ))
    (else (odds-helper (bf sent) (+ counter 1)))
  )
)

(define (odds sent)
  (odds-helper sent 1)
)

; 14.5 aggregate pattern
(define (letter-count sent)
  (if (empty? sent) 0
    (+ (count (first sent)) (letter-count (bf sent)))
  )
)

; 14.6 re-write the function `member?`

(define (member-v2? item to-check)
  (cond
    ((empty? to-check) #f)
    ((equal? item (first to-check)) #t)
    (else (member-v2? item (bf to-check)))
  )
)

; 14.7 every with helper function

(define (differences-helper sent1 sent2)
  (if (empty? sent1) '()
    (se (- (first sent1) (first sent2)) (differences-helper (bf sent1) (bf sent2)))
  )
)

(define (differences sent)
  (differences-helper (bf sent) (bl sent))
)

; 14.8
(define (multi-words num word)
  (if (= num 0) '()
    (se word (multi-words (- num 1) word))
  )
)

(define (expanding sent)
  (cond
    ((empty? sent) '())
    ((= (count sent) 1) (first sent))
    ((number? (first sent))
      (se (multi-words (first sent) (first (bf sent))) (expanding (bf (bf sent))))
    )
    (else 
      (se (first sent) (expanding (bf sent)))
    )
  )
)

; 14.9

(define (location-helper wd sent counter)
  (cond
    ((empty? sent) counter)
    ((equal? wd (first sent)) (+ counter 1))
    (else
      (location-helper wd (bf sent) (+ counter 1))
    )
  )
)

(define (location wd sent)
  (if (member? wd sent) (location-helper wd sent 0) #f)
)

; 14.10 using combining pattern (keep and aggregate pattern)

(define (adjacent-duplicates sent)
  (cond
    ((<= (count sent) 1) '())
    ((equal? (first sent) (first (bf sent))) 
      (se (first sent) (adjacent-duplicates (bf sent)))
    )
    (else (adjacent-duplicates (bf sent)))
  )
)

(define (count-adjacent-duplicates sent)
  (count (adjacent-duplicates sent))
)

; 14.11
(define (remove-adjacent-duplicates-helper sent sentResult)
  (cond
    ((<= (count sent) 1) sentResult)
    ((empty? sentResult) 
      (remove-adjacent-duplicates-helper (bf sent) (se (first sent)))
    )
    ((equal? (first sent) (last sentResult))
      (remove-adjacent-duplicates-helper (bf sent) sentResult)
    )
    (else
      (remove-adjacent-duplicates-helper (bf sent) (se sentResult (first sent)))
    )
  )
)

(define (remove-adjacent-duplicates sent)
  (remove-adjacent-duplicates-helper sent '())
)

; 14.12
(define (progressive-squares? sent)
  (cond
    ((<= (count sent) 1) #t)
    ((= (* (first sent) (first sent)) (first (bf sent)))
      (progressive-squares? (bf sent))
    )
    (else #f)
  )
)

; 14.13
(define (pigl-helper wd counter)
  (cond
    ((= counter 0) (word wd 'ay))
    ((member? (first wd) 'aeiou) (word wd 'ay))
    (else
      (pigl-helper (word (bf wd) (first wd)) (- counter 1))
    )
  )
)

(define (pigl wd)
  (pigl-helper wd (count wd))
)

; 14.14
(define (same-shape-helper sent1 sent2)
  (cond
    ((empty? sent1) #t)
    ((= (count (first sent1)) (count (first sent2)))
      (same-shape-helper (bf sent1) (bf sent2))
    )
    (else #f)
  )
)

(define (same-shape? sent1 sent2)
  (if
    (= (count sent1) (count sent2))
    (same-shape-helper sent1 sent2)
    #f
  )
)

; 14.15
(define (merge-num-helper sent num sortSent)
  (cond
    ((empty? sent) (se sortSent num))
    ((< num (first sent)) (se sortSent num sent))
    (else
      (merge-num-helper (bf sent) num (se sortSent (first sent)))
    )
  )
)

(define (merge-num sent num)
  (cond
    ((< num (first sent)) (se num sent))
    ((> num (last sent)) (se sent num))
    (else 
      (merge-num-helper (bf sent) num (se (first sent)))
    )
  )
)

(merge-num '(4 7 18 40 99) 19)

(define (merge sent1 sent2)
  (cond
    ((empty? sent1) sent2)
    ((empty? sent2) sent1)
    (else
      (merge (merge-num sent1 (first sent2)) (bf sent2))
    )
  )
)

; another solution
(define (merge sent1 sent2)
  (cond
    ((empty? sent1) sent2)
    ((empty? sent2) sent1)
    ((before? (first sent1) (first sent2))
      (se (first sent1) (merge (bf sent1) sent2))
    )
    (else
      (se (first sent2) (merge sent1 (bf sent2)))
    )
  )
)

(merge '(4 7 18 40 99) '(19 25 30 44))
(merge '(4 7 18 40 99) '(3 6 9 12 24 36 50))

; 14.16

(define (vowel? letter)
  (member? letter 'oauei)
)

(define (syllables-helper wd counter counter-vowel)
  (cond
    ((empty? wd) counter-vowel)
    ((= counter 0)
      (if (vowel? (first wd)) 
          (syllables-helper (bf wd) (+ counter 1) (+ counter-vowel 1))
          (syllables-helper (bf wd) counter counter-vowel)
      )
    )
    (else
      (if (vowel? (first wd))
          (syllables-helper (bf wd) (+ counter 1) counter-vowel)
          (syllables-helper (bf wd) 0 counter-vowel)
      )
    )
  )
)

(define (syllables wd)
  (syllables-helper wd 0 0)
)

(syllables 'soaring)
(syllables 'teach)
(syllables 'teachie)
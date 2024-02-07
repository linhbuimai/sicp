(require (planet dyoo/simply-scheme:1:2/simply-scheme))


; Example of functions as data

(define (make-conjugator prefix ending)
  (lambda (verb) (sentence prefix (word verb ending))))

(define third-person (make-conjugator 'she 's))
(define third-person-plural-past (make-conjugator 'they 'ed))
(define second-person-future-perfect
  (make-conjugator '(you will have) 'ed))

; every
(define (first-letters sent)
  (every first sent))

(define (plural noun)
  (if (equal? (last noun) 'y)
      (word (bl noun) 'ies)
      (word noun 's)))

(every plural '(beatle turtle holly kink zombie))

(define (hyphenate word1 word2)
  (word word1 '- word2))

; write function to only add up number in the sentence
(define (add-numbers sent)
  (accumulate + (keep number? sent)))

; implement count: count number of words in a sentence or number of letters in a word
(define (always-one arg)
  1)

(define (count sent)
  (accumulate + (every always-one sent)))

; acronym procedure?
(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define (acronym phrase)
  (accumulate word (every first (keep real-word? phrase))))

; procedure returns another procedure
(define (double sent)
  (se sent sent))

(define (square x)
  (* x x))

((repeated double 3) '(banana))

; use repeated to get a particular element of a sentence
(define (item n sent)
  (first ((repeated bf (- n 1)) sent)))

; use item function, to create another function that spell number
(define (spell-digit digit)
  (item (+ 1 digit)
        '(zero one two three four five six seven eight nine)))

; using: (every spell-digit 1971)

; write a function that access if a sentence have number?
(define (any-numbers? sent)
  (not (empty? (keep number? sent))))
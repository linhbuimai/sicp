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

;; ----- Exercises -----
; 8.4
(define (vowels? letter)
  (member? letter 'aeiou))

(define (choose-beatles func)
  (accumulate sentence (keep func '(john paul george ringo))))

(define (ends-vowel? wd) (vowels? (last wd)))
(define (even-count? wd) (even? (count wd)))

(choose-beatles ends-vowel?)
(choose-beatles even-count?)

; 8.5
(define (transform-beatles func)
  (every func '(john paul george ringo)))

(define (amazify name)
  (word 'the-amazing- name))

(transform-beatles amazify)
(transform-beatles butfirst)

; 8.6

(define (map-single-letter letter)
  (cond ((equal? letter 'a) 'alpha)
        ((equal? letter 'b) 'bravo)
        ((equal? letter 'c) 'credit)
        ((equal? letter 'd) 'direct)
        ((equal? letter 'e) 'elector)
        ((equal? letter 'f) 'foster)
        (else 'not-defined-yet)))

(define (words wds)
  (every map-single-letter wds))

; 8.7
(define (always-one arg)
  1)

(define (count sent)
  (accumulate + (every always-one sent)))

(define (letter-count sent)
  (accumulate + (every count sent)))

; 8.8
(define (helper-exagg wd)
  (cond ((number? wd) (* 2 wd))
        ((equal? wd 'good) 'great)
        ((equal? wd 'bad) 'terrible)
        (else wd)))

(define (exaggerate sent)
  (every helper-exagg sent))

; 8.9
; Use word as the first argument to every
; Use words? as the first argument to keep
; Use sentence as the first argument to accumulate

; 8.10
(define (true-for-all? func sent)
  (let ((filter-sent (keep func sent)))
    (if (equal? sent filter-sent) #t #f)))

; 8.11
(define (base-grade grade)
  (cond ((equal? grade 'A) 4)
        ((equal? grade 'B) 3)
        ((equal? grade 'C) 2)
        ((equal? grade 'D) 1)
        ((equal? grade 'F) 0)
        (else 0)))

(define (grade-modifier grade)
  (cond ((equal? (last grade) '+) (+ (base-grade (first grade)) 0.33))
        ((equal? (last grade) '-) (- (base-grade (first grade)) 0.33))
        (else (base-grade (first grade)))))


(define (count-sent sent)
  (accumulate + (every always-one sent)))

(define (gpa sent)
  (/ (accumulate + (every grade-modifier sent))
     (count-sent sent)))


; 8.12
(define (um? wd)
  (equal? wd 'um))

(define (count-ums sent)
  (let ((filter-sent (keep um? sent)))
    (count filter-sent)))

; 8.13
(define (number-for-letter l)
  (cond ((member? l 'abc) 2)
        ((member? l 'def) 3)
        ((member? l 'ghi) 4)
        ((member? l 'jkl) 5)
        ((member? l 'mno) 6)
        ((member? l 'pqrs) 7)
        ((member? l 'tuv) 8)
        ((member? l 'wxyz) 9)
        (else 0)))

(define (phone-unspell wd)
  (accumulate word (every number-for-letter wd)))

; 8.14
; get the nth item in the sent, sent could be a sentence or a word
(define (item n sent)
  (first ((repeated bf (- n 1)) sent)))

; get the letters between from-to position of a word
(define (subword wd from to)
  (let ; like a sub-proc
    ( (wd_len (count wd)) )
    ( (repeated bl (- wd_len to)) ((repeated bf (- from 1)) wd) )
  )
)
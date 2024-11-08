(require (planet dyoo/simply-scheme:1:2/simply-scheme))

; 12.5
(define (exag-helper wd)
  (cond
    ((number? wd) (* wd 2))
    ((equal? wd 'good) 'great)
    ((equal? wd 'bad) 'terrible)
    (else wd)
  )
)

(define (exaggregate sent)
  (if (empty? sent) '()
    (se (exag-helper (first sent)) (exaggregate (bf sent)))
  )
)

; 12.6
(define (base-grade grd)
  (cond 
    ((equal? grd 'A) 4)
    ((equal? grd 'B) 3)
    ((equal? grd 'C) 2)
    ((equal? grd 'D) 1)
    (else 0)
  )
)

(define (grade-modifier mod)
  (cond
    ((equal? mod '+) 0.33)
    ((equal? mod '-) -0.33)
    (else 0)
  )
)

(define (grade-cal grd)
  (+ (base-grade (first grd)) (grade-modifier (bf grd)))
)

(define (gpa-helper sent)
  (if (= (count sent) 1)
    (grade-cal (first sent))
    (+ (grade-cal (first sent)) (gpa-helper (bf sent)))
  )
)

(define (gpa sent)
  (/ (gpa-helper sent) (count sent))
)

; 12.7
(define (spell-digit digit)
  (item (+ 1 digit)
    '(zero one two three four five six seven eight nine)
  )
)

(define (spell-number num)
  (if (number? num)
    (se (spell-digit (first num)) (spell-number (bf num)))
    '()
  )
)

; 12.8
(define (keep-number wd)
  (if (number? wd) wd '())
)

(define (numbers sent)
  (if (empty? sent) 
    '()
    (se (keep-number (first sent)) (numbers (bf sent)))
  )
)

; 12.9
(define (real-word? wd)
  (if (member? wd '(a the an in of and for  to with)) '() wd)
)

(define (real-words sent)
  (if (empty? sent)
    '()
    (se (real-word? (first sent)) (real-words (bf sent)))
  )
)

; 12.10
(define (check-word wd1 wd2)
  (if (equal? wd1 wd2) '() wd2)
)

(define (remove-wd wd sent)
  (if (empty? sent) 
    '()
    (se (check-word wd (first sent)) (remove-wd wd (bf sent)))
  )
)

; 12.11 re-write count function
(define (always-one arg) 1)

(define (count_v2 wd_sent)
  (if (empty? wd_sent) 
    0
    (+ (always-one (first wd_sent)) (count_v2 (bf wd_sent)))
  )
)

; 12.12
; rule: if the current value is less than the next value
; --> subtract the current value from the total 
; else, add the current value to the total
(define (roman-value letter)
  (cond
    ((equal? letter 'i) 1)
    ((equal? letter 'v) 5)
    ((equal? letter 'x) 10)
    ((equal? letter 'l) 50)
    ((equal? letter 'c) 100)
    ((equal? letter 'd) 500)
    ((equal? letter 'm) 1000)
    (else 0)
  )
)

(define (map-roman-value wd)
  (if (empty? wd) '()
    (se (roman-value (first wd)) (map-roman-value (bf wd)))
  )
)


(define (arabic-helper sent)
  (cond
    ((empty? sent) '())
    ((= (count sent) 1) (first sent))
    (else 
      (+ (if (< (first sent) (first (bf sent))) (- 0 (first sent)) (first sent))
        (arabic-helper (bf sent))
      ))
))

(define (arabic wd)
  (arabic-helper (map-roman-value wd))
)

; 12.13

; -- function to get the remain: `remainder`
; -- function to get the quotient: `quotient`

(define seconds 60) ; = 1 minute
(define minutes 60) ; = 1 hour
(define hours 24)   ; = 1 day
(define days 7)     ; = 1 week
(define weeks 52)   ; = 1 year
(define years 100)  ; = 1 century

(define testcase1 22222)
(define testcase2 4967189641)
(define testcase3 8967189641)

(define
  time-mapping '(60 60 24 7 52 100)
)

(define
  time-display-mapping '(century year week day hour minute second)
)

(define (add-plural num text)
  (if 
    (<= num 1)  text
    (if 
      (equal? text 'century)
      'centuries
      (word text 's)
    )
  )
)

(define (time-divide num time-mapping)
  (if 
    (= (count time-mapping) 1)
    (se (remainder num (first time-mapping)) (quotient num (first time-mapping)) )
    (se 
      (remainder num (first time-mapping))
      (time-divide (quotient num (first time-mapping)) (bf time-mapping))
    )
  )
)

(define (mapping-division time-sent time-text)
  (if
    (= (count time-text) 1)
    (se (first time-sent) (add-plural (first time-sent) (first time-text)))

    (if 
      (= (first time-sent) 0)
      (mapping-division (bf time-sent) (bf time-text))
      (se (first time-sent) (add-plural (first time-sent) (first time-text)) (mapping-division (bf time-sent) (bf time-text)))
    )
  )
)

(define (describe-time num)
  (mapping-division (reverse (time-divide num time-mapping)) time-display-mapping)
)

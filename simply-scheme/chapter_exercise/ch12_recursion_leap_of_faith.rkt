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

(1000 100 50 10 1 1)       --> 1162
(1000 100 1000 50 10 1 1)  --> (1000 -100 1000 50 1 1)


; 12.13
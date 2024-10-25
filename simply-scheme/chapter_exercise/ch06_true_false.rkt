(require (planet dyoo/simply-scheme:1:2/simply-scheme))

; example of giving decision
(define (greet name)
  (if (equal? (first name) 'professor)
      (se '(i hope i am not bothering you) 'professor (last name))
      (se '(good to see you) (first name))))

(greet '(matt wright))
(greet '(professor harold abelson))

; example of predicates

(define (divisible? big little)
  (= (remainder big little) 0))

(define (buzz num)
  (if (or (divisible? num 7) (member? 7 num))
      'buzz
      num))

; semipredicates
(define (integer-quotient big little)
  (if (divisible? big little)
	  (/ big little)
	  #f))

(define (integer-quotient-wa big little)
  (and (divisible? big little)
       (/ big little)))

; multiple case with cond
(define (roman-value letter)
  (cond ((equal? letter 'i) 1)
        ((equal? letter 'v) 5)
        ((equal? letter 'x) 10)
        ((equal? letter 'l) 50)
        ((equal? letter 'c) 100)
        ((equal? letter 'd) 500)
        ((equal? letter 'm) 1000)
        (else 'huh?)))

(define (truefalse value)
  (cond (value 'true)
        (else 'false)))

; if is composable
(define (greetv1 name)
  (if (equal? (first name) 'professor)
      (se '(pleased to meet you)
          'professor
          (last name)
          '(-- how are you?))
      (se '(pleased to meet you)
          (first name)
          '(-- how are you?))))

(define (greetv2 name)
  (se '(pleased to meet you)
      (if (equal? (first name) 'professor)
          (se 'professor (last name))
          (se (first name)))
      '(-- how are you?)))

;; Exercises
; 6.5
(define (european-time sent)
  (cond ((= (first sent) 12) (cond ((equal? (last sent) 'am) 24)
                                   ((equal? (last sent) 'pm) 12)
                                   (else 'huh?)))
        ((equal? (last sent) 'am) (first sent))
        ((equal? (last sent) 'pm) (+ (first sent) 12))
        (else 'huh?))
  )

(define (american-time num)
  (cond ((= num 12) (se num 'pm))
        ((= num 24) (se 12 'am))
        ((< num 12) (se num 'am))
        ((> num 12) (se (- num 12) 'pm))
        (else 'huh?)))

; 6.6
(define (teen? num)
  (and (>= num 13) (<= num 19) #t))

; 6.7
(define (type-of args)
  (cond ((number? args) 'number)
        ((word? args) 'word)
        ((sentence? args) 'sentence)
        ((boolean? args) 'boolean)
        (else 'huh?)))

; 6.8
(define (vowel? letter)
  (member? letter 'aeiou))

(define (indef-article wd)
  (if (vowel? (first wd))
      (sentence 'an wd)
      (sentence 'a wd)))

; 6.9
(define (to-plural wd)
  (if (equal? (last wd) 'y)
      (word (bl wd) 'ies)
      (word wd 's)))

(define (thismany num wd)
  (if (> num 1)
      (se num (to-plural wd))
      (se num wd)))

; 6.10
(define (sort2 nums)
  (cond ((< (first nums) (last nums)) nums)
        (else (se (last nums) (first nums)))))

; 6.11
(define (month31? num)
  (member? num '(1 3 5 7 8 10 12)))

(define (month30? num)
  (member? num '(4 6 9 11)))

(define (leapday? num)
  (cond ((and (divisible? num 100) (divisible? num 400)) #t)
        ((and (divisible? num 100) (not (divisible? num 400))) #f)
        ((divisible? num 4) #t)
        (else #f)))

(define (check-threshold? num lower upper)
  (and (>= num lower) (<= num upper)))

(define (valid-date? mo da ye)
  (cond ((and (= mo 2) (= da 29)) (leapday? ye))
        ((and (month31? mo)
              (check-threshold? da 1 31)
              (> ye 0)) #t)
        ((and (month30? mo)
              (check-threshold? da 1 30)
              (> ye 0)) #t)
        ((and (= mo 2)
              (check-threshold? da 1 28)
              (> ye 0)) #t)
        (else #f)
        )
  )
        

;(valid-date? 10 4 1949)
;(valid-date? 20 4 1776)
;(valid-date? 5 0 1992)
;(valid-date? 2 29 1900)
;(valid-date? 2 29 2000)

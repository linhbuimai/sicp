(require (planet dyoo/simply-scheme:1:2/simply-scheme))

;; 15.1

(define (to-binary-helper num wd)
  (if (<= num 1)
    (word num wd)
    (word (to-binary-helper (quotient num 2) (word (remainder num 2) wd)))
  )
)

(define (to-binary num)
  (to-binary-helper num "")
)

;; 15.2

(define (combine-sentence sent wd)
  (if (empty? sent)
    wd
    (combine-sentence (bf sent) (word wd (first sent)))
  )
)

(define (palindrome-helper wd)
  (cond
    ((<= (count wd) 1) #t)
    ((equal? (first wd) (last wd)) (palindrome-helper (bf (bl wd))))
    (else #f)
  )
)

(define (palindrome? sent)
  (let ((wd (combine-sentence sent "")))
    (palindrome-helper wd)
  )
)

;; 15.3
; procedure that returns all substrings of a word

(define (substrings-helper wd)
  (if (empty? wd)
    '()
    (se wd (substrings-helper (bl wd)))
  )
)

(define (substrings wd)
  (if (empty? wd)
    (se '())
    (let ((var-substr (substrings-helper wd)))
      (se var-substr (substrings (bf wd)))
    )
  )
)

;; 15.4
(define (substrings? wd1 wd)
  (let ((all-substr (substrings wd)))
    (member? wd1 all-substr)
  )
)

(substrings? 'ssip 'mississippi)
(substrings? 'misip 'mississippi)

;; 15.5
(define (letters n)
  (cond ((= n 2) (se 'a 'b 'c))
        ((= n 3) (se 'd 'e 'f))
        ((= n 4) (se 'g 'h 'i))
        ((= n 5) (se 'j 'k 'l))
        ((= n 6) (se 'm 'n 'o))
        ((= n 7) (se 'p 'q 'r 's))
        ((= n 8) (se 't 'u 'v))
        ((= n 9) (se 'w 'x 'y 'z))
        (else n)))
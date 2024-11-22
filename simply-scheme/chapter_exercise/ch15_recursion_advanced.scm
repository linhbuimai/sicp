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

(define (prepend-every letter sent)
  (if (empty? sent)
    (se letter '())
    (se (word letter (first sent))
      (prepend-every letter (bf sent)) 
    )
  )
)

(define (substrings wd)
  (cond 
    ((= (count wd) 1) (se wd))
    ((= (count wd) 2) (se (first wd) wd (last wd)))
    (else
      
    )
  )
)
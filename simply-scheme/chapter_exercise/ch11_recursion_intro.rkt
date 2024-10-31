(require (planet dyoo/simply-scheme:1:2/simply-scheme))

; 11.5
(define (get-first-initial sent)
  (first (first sent))
)

(define (initials sent)
  (if (empty? sent)
    '()
    (se (get-first-initial sent) (initials (bf sent)))
  )
)

; 11.6
(define (countdown-helper num)
  (if (<= num 0) '() (se num 'BLASTOFF!))
)

(define (countdown num)
  (if (<= num 1)
    (countdown-helper num)
    (se num (countdown (- num 1)))
  )
)

; 11.7
(define (copies num wd)
  (if (or (<= num 0) (empty? wd))
    '()
    (se wd (copies (- num 1) wd))
  )
)
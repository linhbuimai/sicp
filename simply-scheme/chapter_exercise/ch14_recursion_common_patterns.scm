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

; 14.10

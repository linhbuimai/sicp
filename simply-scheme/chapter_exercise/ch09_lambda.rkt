(require (planet dyoo/simply-scheme:1:2/simply-scheme))

;; Chapter 09 exercises

;; 9.4
(define (who sent)
  (every (lambda (person) (se person sent))
   '(pete roger john keith))
)

;; 9.5
; Write prepend-every

(define (prepend-every wd sent)
  (every (lambda (each-wd) (word wd each-wd)) sent)
)

(prepend-every 's '(he aid he aid))
(prepend-every 'anti '(dote pasto gone body))

;; 9.6
(define (square x)
  (* x x))

(define (sentence-version fn)
  (lambda (sent)
    (every fn sent)))

  (define (sentence-version F) (lambda (sent) (every F sent)))
; test
; ((sentence-version first) '(if i fell))
; ((sentence-version square) '(2 4 5 7))
; ((sentence-version (lambda (x) (+ x 1))) '(1 2 3 4)')

;; 9.7
(define (letterwords letter sent)
  (keep (lambda (wd) (member? letter wd)) sent))
; test
; (letterwords 'o '(got to get you into my life))

;; 9.8

(define (hang-letter letter guesses)
  (if (member? letter guesses) letter '_)
)

(define (hang wd letters)
  (every (lambda (letter) (hang-letter letter letters)) wd))

;; 9.9
(define (common-words sent1 sent2)
  (keep (lambda (wd) (member? wd sent2)) sent1)
)

;; 9.10
(define (count sent)
  (accumulate + (every (lambda (arg) 1) sent)))

(define (appearances wd sent)
  (count (keep (lambda (wd1) (equal? wd1 wd)) sent))
)

(appearances 'hihi '(hihi ha hihi hoh ha hihi))

;; 9.11

(define (item n sent)
  (first ((repeated bf (- n 1)) sent))
)
(define (unabbrev sent1 sent2) 
  (every (lambda (wd) (if (number? wd) (item wd sent2) wd)) sent1)
)

; test function
(unabbrev '(john 1 wayne fred 4) '(bill hank kermit joey))
(unabbrev '(i 3 4 tell 2) '(do you want to know a secret?))

;; 9.12
(define (first-last sent)
  (keep (lambda (wd) (equal? (first wd) (last wd))) sent)
)

(first-last '(california ohio nebraska alabama alaska massachusetts))

;; 9.13
(define (compose F G)
  (lambda (x) (F (G x)))
)

((compose first butfirst) '(higher order function))
((compose sqrt abs) -25)

;; 9.14
(define (substitute wd1 wd2 sent)
  (every (lambda (wd) (if (equal? wd wd2) wd1 wd)) sent)
)

(substitute 'maybe 'yeah '(she loves you yeah yeah yeah))

;; 9.15
(define (type-check func pred?)
  (lambda (arg) (
    if (pred? arg) (func arg) #f
  ))
)

(define safe-sqrt (type-check sqrt number?))

;; 9.16

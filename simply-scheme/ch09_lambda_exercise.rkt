(require (planet dyoo/simply-scheme:1:2/simply-scheme))

;; Chapter 09 exercises

;; 9.4
(define (who sent)
  (every
   (lambda (person) (se person sent))
   '(pete roger john keith)))

;; 9.5
; Write prepend-every

(define (prepend-every wd sent)
  (every
   (lambda (each-wd) (word wd each-wd))
   sent))

(prepend-every 's '(he aid he aid))
(prepend-every 'anti '(dote pasto gone body))

;; 9.6
(define (square x)
  (* x x))

(define (sentence-version fn)
  (lambda (sent)
    (every fn sent)))
; test
; ((sentence-version first) '(if i fell))
; ((sentence-version square) '(2 4 5 7))

;; 9.7
(define (letterwords letter sent)
  (keep (lambda (wd) (member? letter wd)) sent))
; test
; (letterwords 'o '(got to get you into my life))

;; 9.8

(define (hang-letter letter guesses)
  (if (member? letter guesses)
      letter
      '_))

(define (hang wd letters)
  (every (lambda (letter) (hang-letter letter letters)) wd))

;; 9.9
(define (check-wd? wd sent)
  (if (member? wd sent)
      #t
      #f))
(define (common-words sent1 sent2)
  (keep (lambda (wd) (check-wd? wd sent1)) sent2))

;; 9.10

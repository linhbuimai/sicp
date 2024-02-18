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


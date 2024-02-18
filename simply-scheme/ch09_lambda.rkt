(require (planet dyoo/simply-scheme:1:2/simply-scheme))

; lambda example
(define (add-three-to-each sent)
  (every (lambda (number) (+ number 3)) sent))

(every (lambda (wd) (se (first wd) wd (last wd)))
       '(only a northern song))

(keep (lambda (n) (member? 9 n))
      '(4 81 909 781 1969 1776))

(accumulate (lambda (this that)
              (if (> (count this) (count that)) this that))
            '(wild honey pie))

(keep (lambda (person)
        (member? person '(john paul george ringo)))
      '(mick smokey paul diana bill geddy john yoko keith reparata))

(keep (lambda (person)
        (member? 'e person))
      '(mick smokey paul diana bill geddy john yoko keith reparata))

; procedures that return procedures
(define (same-arg-twice fn)
  (lambda (arg) (fn arg arg)))

(define (flip fn)
  (lambda (a b) (fn b a)))

(define square (same-arg-twice *))
(define fourth-power (repeated square 2))

; example of using lambda instead of let
(define (roots a b c)
  ((lambda (discriminant)
     (se (/ (+ (- b) discriminant) (* 2 a))
         (/ (- (- b) discriminant) (* 2 a))))
   ; the following code is value of parameter discriminant --> invoke lambda function
   (sqrt (- (* b b) (* 4 a c))))
  )

(define (backwards wd)
  (accumulate (lambda (a b) (word b a)) wd))

(backwards 'yesterday)
(every backwards '(i saw her standing there))

; examples
;(define (keep-h sent)
;  (keep (lambda (wd) (member 'h wd)) sent))

(define (keeper letter)
  (lambda (sent)
    (keep (lambda (wd) (member? letter wd)) sent)))
;; can use as:
; ((keeper 'h) '(this is another example))
; >> (this another)

;; can se keeper to define keep-h
(define keep-h (keeper 'h))
; (keep-h '(this is another example))
; >> (this another)

;; comment: keeper is a generalize function of keep-h
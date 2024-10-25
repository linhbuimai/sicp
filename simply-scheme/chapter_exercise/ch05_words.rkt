(require (planet dyoo/simply-scheme:1:2/simply-scheme))

(define (get-second thing)
  (first (butfirst thing)))

(item 4 '(being for the benefit of mister kite!))

; 5.14
(define (get-third thing)
  (first (butfirst (butfirst thing))))

(define (get-third-item thing)
  (item 3 thing))

; 5.15
(define (first-two wd)
  (word (first wd) (first (bf wd))))

(define (first-two-item wd)
  (word (item 1 wd) (item 2 wd)))

; 5.16
(define (two-first wd1 wd2)
  (word (first wd1) (first wd2)))

(define (two-first-sent snt)
  (word (first (first snt))
        (first (first (bf snt)))))

; 5.17
(define (knight snt)
  (sentence 'Sir snt))

; 5.18
; (define (ends word)
;   (word (first word) (last word)))

; Wrong because use word is procedure name as parameter name.

; 5.19
(define (insert-and snt)
  (sentence (butlast snt)
            'and
            (last snt)
            ))

(insert-and '(john bill wayne fred joey))

; 5.20
(define (middle-names snt)
  (butlast (butfirst snt)))

(middle-names '(james paul mccartney))
(middle-names '(john ronald raoul tolkien))
(middle-names '(bugs bunny))

; 5.21
(define (query snt)
  (sentence (item 2 snt)
            (first snt)
            (butfirst (butfirst (butlast snt)))
            (word (last snt) '?)
            ))

(query '(you are experienced))
(query '(i should have known better))
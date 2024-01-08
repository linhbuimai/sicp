(require (planet dyoo/simply-scheme:1:2/simply-scheme))

; Simple functions examples
(define (real-word? wd)
  (not (member? wd '(a the an in of and for  to with))))

(define (acronym phrase)
  (accumulate word (every first (keep real-word? phrase))))

(acronym '(united states of america))

; Example: Latin Pig

(define (pigl wd)
  (if (member? (first wd) 'aeiou)
      (word wd 'ay)
      (pigl (word (butfirst wd)(first wd)))))

(pigl 'spaghetti)
(pigl 'ok)
(every pigl '(the ballad of john and yoko))

; Example: Ice Cream Choices
(define (choices menu)
  (if (null? menu)
      '(())
      (let ((smaller (choices (cdr menu))))
        (reduce append
                (map (lambda (item) (prepend-every item smaller))
                     (car menu))))))

(define (prepend-every item lst)
  (map (lambda (choice) (se item choice)) lst))

(choices '((small medium large)
           (vanilla (ultra chocolate) (rum raisin) ginger)
           (cone cup)) )

; Example: Combinations from a  Set
(define (combinations size set)
  (cond ((= size 0) '(()))
        ((empty? set) '())
        (else (append (prepend-every (first set)
                                     (combinations (- size 1)
                                                  (butfirst set)))
                      (combinations size (butfirst set))))))

(combinations 3 '(a b c d e))

; Example: Factorial
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5)
(require (planet dyoo/simply-scheme:1:2/simply-scheme))

(define (acronym phrase)
  (accumulate word (every first phrase)))

(acronym '(american civil liberties union))
(every first '(american civil liberties union))

(acronym '(structure and interpretation of computer programs))

(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(acronym '(structure and interpretation of computer programs))
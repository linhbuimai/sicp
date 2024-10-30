(require (planet dyoo/simply-scheme:1:2/simply-scheme))

;; -- find-triples
(define (substitute-letter square position)
  (if (equal? '_ (item square position)) square (item square position))
)

(define (substitute-triple combination position)
  (accumulate word
    (every 
      (lambda (square) (substitute-letter square position))
      combination
    )
  )
)

(define (find-triples position)
  (every (lambda (comb) (substitute-triple comb position))
    '(123 456 789 147 258 369 159 357))
)

;; -- check i-can-win?
(define (opponent letter) (if (equal? letter 'x) 'o 'x))

;; procs to check if a triple is the possible winner?
(define (my-pair? triple me)
  (and (= (appearances me triple) 2)
    (= (appearances (opponent me) triple) 0))
)

;; choose-win returns the number of the square (ô vuông) that leads to win
(define (choose-win winning-triples)
  (if (empty? winning-tripples) #f (keep number? (first winning-tripples)))
)

(define (i-can-win? triples me)
  (choose-win
    (keep (lambda (triple) (my-pair? triple me)) triples) ; only keep the triple having possiple win
  )
)

;; -- check opponent-can-win?
(define (opponent-can-win? triples me) (i-can-win? tripples (opponent me)))

;; -- check i-can-fork?
(define (my-single? triple me)
  (and (= (appearances me triple) 1)
    (= (appearances (opponent me) triple) 0))
)

(define (extract-digit desired-digit wd)
  (keep (lambda (wd-digit) (equal? wd-digit desired-digit)) wd)
)

(define (sort-digits number-wd)
  (every (lambda (digit) (extract-digit digit number-wd)) 
    '(1 2 3 4 5 6 7 8 9))
)

(define (repeated-numbers sent)
  (every first
    (keep (lambda (wd) (>= (count wd) 2)) (sort-digits (accumulate word sent)))
  )
)

(define (pivots triples me)
  (repeated-numbers (keep (lambda (triple) (my-single? triple me)) triples))
)

(define (first-if-any sent) (if (empty? sent) #f (first sent)))

(define (i-can-fork? triples me)
  (first-if-any (pivots triples me))
)

;; -- check i-can-advance?
(define (best-square-helper opponent-pivots pair)
  (if (member? (first pair) opponent-pivots)
    (first pair) (last pair))
)

(define (best-square my-triple triples me)
  (best-square-helper (pivots triples (opponent me)) (keep number? my-triple))
)

(define (best-move my-triples all-triples me)
  (if (empty? my-triples) #f (best-square (first my-triples) all-triples me))
)

(define (i-can-advance? triples me)
  (best-move
    (keep (lambda (triple) (my-single? triple me)) triples)
    triples me)
)

;; -- decide leftover
(define (first-choice posibilities preferences)
  (first (keep (lambda (square) (member? square posibilities)) preferences))
)

(define (best-free-square triples)
  (first-choice (accumulate word triples) '(5 1 3 7 9 2 4 6 8))
)

;; -- final combination of the program
(define (ttt-choose triples me)
  (cond 
    ((i-can-win? triples me))
    ((opponent-can-win? triples me))
    ((i-can-fork? triples me))
    ((i-can-advance? triples me))
    (else (best-free-square triples))
  )
)

(define (ttt position me)
  (ttt-choose (find-triples position) me)
)

;; ---------- Do the Exercises ---------- ;;
(define (helper-check-won triple letter)
  (or (my-pair? triple letter) (= (appearances letter triple) 3))
)

(define (check-won triples letter)
  (keep (lambda (triple) (helper-check-won triple letter)) triples)
)

(define (already-won? position me)
  (if (empty? (check-won (find-triples position) me)) #f #t)
)

(define (who-can-win? position)
  (cond 
    ((already-won? position 'x) 'x)
    ((already-won? position 'o) 'o)
    (else #f)
  )
)

(define (tie-game? position)
  (and (not (who-can-win? position)) (<= (appearances '_ position) 1)))

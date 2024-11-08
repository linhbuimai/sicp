(require (planet dyoo/simply-scheme:1:2/simply-scheme))

;; tracing the recursion procedures
(define (downup wd)
  (if (= (count wd) 1)
    (se wd)
    (se wd (downup (bf wd)) wd)
  )
)

(trace downup)
(downup 'linh)

;; complex recursion example
(define (fib n)
  (if (<= n 2)
    1
    (+ (fib (- n 1)) (fib (- n 2)))
  )
)
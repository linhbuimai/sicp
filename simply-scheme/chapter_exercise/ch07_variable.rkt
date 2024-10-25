(require (planet dyoo/simply-scheme:1:2/simply-scheme))

(define (roots-ver1 a b c)
  (let ( (discriminant (sqrt (- (* b b) (* 4 a c)))) )
    (se (/ (+ (- b) discriminant) (* 2 a))
        (/ (- (- b) discriminant) (* 2 a)))))

(define (roots-ver2 a b c)
  (let ( (discriminant (sqrt (- (* b b) (* 4 a c))))
         (minus-b (- b))
         (two-a (* 2 a)))
    (se (/ (+ minus-b discriminant) two-a)
        (/ (- minus-b discriminant) two-a))))

; 7.4
(define (sum-square a b)
  (let ( (+ *)
         (* +))
    (* (+ a a) (+ b b))))
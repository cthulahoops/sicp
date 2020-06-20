(define (square x) (* x x))
(define (double x) (* x 2))

(define (cmp? a b)
  (cond ((> a b) 'gt)
        ((= a b) 'eq)
        (else 'lt)))

(cmp? 5 6)

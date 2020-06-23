(define ones (cons 1 ones))
(define (take n obj) (if (> n 0) (cons (car obj) (take (- n 1) (cdr obj))) '()))

(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
                    (add-lists (cdr list1) (cdr list2))))))

(define integers (cons 1 (add-lists ones integers)))
(define fibs (cons 0 (cons 1 (add-lists fibs (cdr fibs)))))

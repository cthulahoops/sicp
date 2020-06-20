(define (deriv exp var)
  (cond ((number? exp) 0)
    ((variable? exp)
      (if (same-variable? exp var) 1 0))
    ((sum? exp)
      (make-sum (deriv (addend exp) var)
                (deriv (augend exp) var)))
    ((product? exp)
     (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp))))
    ((exponentiation? exp)
      (make-product
        (make-product (exponent exp)
                      (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
        (deriv (base exp) var)))
    (else
      (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a1 0) a2)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((eq? a1 a2) (list '* 2 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((and (exponentiation? m2) (eq? (base m2) m1))
          (make-exponentiation (base m2)
                               (make-sum (exponent m2) 1)))
        ((eq? m1 m2) (list '** m1 2))
        (else (list '* m1 m2))))

(define (mp . xs)
  (cond ((or 
         (else (cons '* xs)))

(define (make-exponentiation b e)
  (cond ((=number? e 1) b)
        ((=number? e 0) 1)
        (else (list '** b e))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (expression? type)
        (lambda (x)
          (and (pair? x)
               (eq? (car x) type))))

(define exponentiation? (expression? '**))

(define (addend s) (cadr s))
(define (augend s)
  (let ((rest (cddr s)))
    (if (null? (cdr rest))
        (car rest)
        (cons '+ rest))))

(define (multiplier p) (cadr p))
(define (multiplicand p)
  (define (mf rest)
    (if (null? (cdr rest))
        (car rest)
        (make-product (car rest) (mf (cdr rest)))))
  (mf (cddr p)))

(define base cadr)
(define exponent caddr)

(define (=number? exp num) (and (number? exp) (= exp num)))

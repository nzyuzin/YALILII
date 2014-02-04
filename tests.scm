(define (test-cons)
  (pair? (cons 1 2)))

(define (test-car)
  (= (car (cons 1 2)) 1))

(define (test-cdr)
  (= (cdr (cons 1 2)) 2))

(define (test-list)
  (define l (list 1 2))
  (and (null? (list)) (pair? l) (null? (cdr (cdr l))) (= (car l) 1)))

(define (test-define)
  (define x 3)
  (= x 3))

(define (test-set!)
  (define x 3)
  (set! x 5)
  (= x 5))

(define (test-if)
  (if true
    true
    false))

(define (test-cond)
  (cond (false 1)
        (true true)
        (default false)))

(define (test-quote)
  (quoted? '(quote text)))

(define (test-math)
  (and (= 2 (- 5 3))
       (= 2 (/ 8 4))
       (= 2 (* 1 2))
       (= 2 (+ 1 1))
       (> 3 2)))

(define (test)
  (and (test-cons)
       (test-car)
       (test-cdr)
       (test-list)
       (test-define)
       (test-set!)
       (test-if)
       (test-cond)
       (test-quote)
       (test-math)))

(test)

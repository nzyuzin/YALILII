(define (test-func body)
  (define (helper-func expr def-names def-values rest-of-expr)
    (if (null? expr)
      (list rest-of-expr def-names def-values)
      (if (definition? (car expr))
        (helper-func 
          (cdr expr)
          (cons (definition-variable def-names) def-names)
          (cons (definition-value def-values) def-values)
          rest-of-expr)
        (helper-func (cdr expr) def-names def-values (append rest-of-expr (list (car expr)))))))
  (let ((scanned-out (helper-func body (list) (list) (list)))
        (define-var-vals '*unassigned*)
        (rest-of-body '*unassigned*))
    (set! define-var-vals (cdr scanned-out))
    (set! rest-of-body (car scanned-out))
    (display scanned-out)
    (if (null? (car define-var-vals))
      rest-of-body
    (list 
     'let
     (map (lambda (x) (cons x '*unassigned*)) (car define-var-vals))
     (map (lambda (x) (list (car x) (cadr x)) define-var-vals))
     rest-of-body))))

(define test-list '((define u 1) (define v 2) (+ 4 5)))

(scan-out-defines test-list)

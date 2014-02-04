;(define (f n)
;  (define (x 1))
;    (+ x n))

;(f 5)

(define sss '(
  (define u x)
  (define v y)
  (+ x y)))

(load "basic_interpreter.scm")

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (equal? (car vals) "*unassigned*")
                 (error "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;(define (scan-out-defines body)
; (let ((let-body '())
;       (set-body '())
;       (others '()))
;   (let scan-iter ((b body))
;     (cond ((null? b)
;            '())
;           ((definition? (car b))
;            (let ((def-var (definition-variable (car b)))
;                  (def-val (definition-value (car b))))
;              (set! let-body (cons (list def-var ''*unassigned*) 
;                                   let-body))
;              (set! set-body (cons (cons 'set! (list def-var def-val))
;                                   set-body))))
;           (else (set! others (append others (list (car b))))))
;     (if (not (null? b))
;         (scan-iter (cdr b))))
;   (if (null? let-body)
;       body
;       (list (append (list 'let let-body) (append set-body others))))))

(define (scan-out-defines body)
  (let
    ((scanned-out (fetch-defines body '() '() '()))
     (define-vars '*unassigned*)
     (define-vals '*unassigned*)
     (rest-of-body '*unassigned*))
    (set! define-vars (cadr scanned-out))
    (set! define-vals (caddr scanned-out))
    (set! rest-of-body (car scanned-out))
    (if (null? define-vars)
      rest-of-body
      (append (list
                'let
                (map (lambda (x) (list x ''*unassigned*)) define-vars))
              (map (lambda (x y) (list 'set! x y)) define-vars define-vals)
              rest-of-body))))

(scan-out-defines sss)

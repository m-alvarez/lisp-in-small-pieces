#lang racket

(require racket/trace)
(require racket/format)

(define (massoc k l)
  (cond
    ((null? l) #f)
    ((and
      (cons? (car l))
      (eq? k (car (car l))))
     (car l))
    ((and
      (mpair? (car l))
      (eq? k (mcar (car l))))
     (car l))
    (#t (massoc k (cdr l)))))

(define (lookup env expr)
  (if (null? env)
      #f
      (let ((entry (massoc expr (car env))))
        (or entry (lookup (cdr env) expr)))))

(define (is-truthy expr)
  (not (eq? expr #f)))

(define (entry-get entry)
  (cond
    ((cons? entry) (cdr entry))
    ((mpair? entry) (mcdr entry))))

(define (ev-err message)
  (cons 'error message))
(define (ev-ok value)
  (cons 'ok value))

(define-syntax let?
  (syntax-rules ()
    ((let? ((var expr) bindings ...) body ...)
     (match expr
       ((cons 'ok var)
        (let? (bindings ...) body ...))
       ((cons 'error msg) expr)
       (anything (error "INTERNAL ERROR IN LET?:" anything))))
    ((let? () body ...)
     (begin body ...))))

(define (ev-variable env expr)
  (if (symbol? expr)
      (let ((entry (lookup env expr)))
        (if entry
            (ev-ok (entry-get entry))
            (ev-err (format "Unbound variable ~v" expr))))
      (ev-err (format "Not a variable name: ~v" expr))))

(define (ev-set! env var expr)
  (if (symbol? var)
      (let ((entry (lookup env var)))
        (cond
          ((mpair? entry)
           (let? ((new-value (evaluate env expr)))
             (set-mcdr! entry new-value)
             (ev-ok new-value)))
          ((cons? entry)
           (ev-err (format "Cannot update mutable variable ~v" var)))
          (#t (ev-err (format "Unbound variable ~v" var)))))
      (ev-err (format "Not a variable name: ~v" expr))))


(define (literal? expr)
  (or (number? expr)
      (boolean? expr)
      (string? expr)
      (char? expr)))
(define (ev-literal expr) (ev-ok expr))
(define (ev-quote body) (ev-ok body))

(define (extend-env env vars vals)
  (define (extend-frame f vars vals)
    (cond
      ((and (null? vars) (null? vals)) f)
      ((or (null? vars) (null? vals))
       (error "INTERNAL ERROR"))
      (#t (extend-frame (cons (mcons (car vars) (car vals)) f) (cdr vars) (cdr vals)))))
  (cons (extend-frame '() vars vals) env))

(define (map-result fn args)
  (if (null? args) (ev-ok '())
      (match (fn (car args))
        ((cons 'ok val)
         (match (map-result fn (cdr args))
           ((cons 'ok vals) (ev-ok (cons val vals)))
           ((cons 'error err) (ev-err err))))
        ((cons 'error err) (ev-err err)))))

(define (ev-let env vars exprs body)
  (let? ((vals (map-result (lambda (expr) (evaluate env expr)) exprs)))
        (ev-block (extend-env env vars vals) body)))

(define (ev-if env cond tt ff)
  (let? ((cond-val (evaluate env cond)))
        (if (is-truthy cond-val)
            (evaluate env tt)
            (evaluate env ff))))

(define (evaluate env expr)
  (cond
    ((symbol? expr)
     (ev-variable env expr))
    ((literal? expr)
     (ev-literal expr))
    ((list? expr)
     (match expr
       ((list 'quote expr)
        (ev-quote expr))
       ((list 'let bindings body ...)
        (let* ((vars (map car bindings))
               (exprs (map cadr bindings)))
          (ev-let env vars exprs body)))
       ((list 'if b tt ff)
        (ev-if env b tt ff))
       ((list 'begin exprs ...)
        (ev-block env exprs))
       ((list 'set! var val)
        (ev-set! env var val))
       ((list 'lambda args body ...)
        (ev-lambda env args body))
       ((list rator rands ...)
        (ev-app env rator rands))))))

(define (ev-block env exprs)
  (cond
    ((null? exprs) (ev-ok '()))
    ((null? (cdr exprs))
     (evaluate env (car exprs)))
    (#t (let? ((_ (evaluate env (car exprs))))
              (ev-block env (cdr exprs))))))

(define (ev-lambda env args body)
  (ev-ok
   (list 'function env args body)))

(define (ev-app env rator rands)
  (let? ((rator (evaluate env rator)))
        (cond
          ((procedure? rator)
           (let? ((vals (map-result (lambda (expr) (evaluate env expr)) rands)))
                 (ev-ok (apply rator vals))))
          ((and (list? rator)
                (eq? 'function (car rator)))
           (let ((closure-env (cadr rator))
                 (args (caddr rator))
                 (body (cadddr rator)))
             (let? ((vals (map-result (lambda (expr) (evaluate env expr)) rands)))
                   (if (= (length vals) (length args))
                       (ev-block (extend-env closure-env args vals) body)
                       (ev-err (format "Incorrect number of arguments for ~v~nExpected ~v, but got ~v" rator args rands))))))
          (#t
           (ev-err (format "I don't know how to invoke ~v" rator))))))

(define-syntax push!
  (syntax-rules ()
    ((push! thing place)
     (set! place (cons thing place)))))
(define *GLOBALS* '())
(define-syntax def-primitive
  (syntax-rules ()
    ((def-primitive (name inputs ...) body ...)
     (push!
      (cons (quote name)
            (let* ()
              (define (name inputs ...) body ...)
              name))
      *GLOBALS*))
    ((def-primitive name value)
     (push! (cons (quote name) value) *GLOBALS*))))
(define-syntax import-primitive
  (syntax-rules ()
    ((import-primitive name)
     (push! (cons (quote name) name) *GLOBALS*))))

;(def-primitive (cons x y) (mcons x y))
;(def-primitive (car l) (mcar l))
;(def-primitive (cdr l) (mcdr l))
;(def-primitive (set-car! l) (set-mcar! l))
;(def-primitive (set-cdr! l) (set-mcdr! l))
;(def-primitive (and x y) (and x y))
;(def-primitive (or x y) (or x y))
;(import-primitive eq?)
;(import-primitive +)
(import-primitive -)
(import-primitive *)
;(import-primitive <)
(import-primitive =)
;(import-primitive not)
(import-primitive display)

(define-syntax mylisp
  (syntax-rules ()
    ((mylisp expr)
     (match (evaluate (list *GLOBALS*) (quote expr))
       ((cons 'ok result) result)
       ((cons 'error err) (display err) (display "\n"))))
    ((mylisp expr exprs ...)
     (match (evaluate (list *GLOBALS*) (quote expr))
       ((cons 'ok _) (mylisp exprs ...))
       ((cons 'error err) (display err) (display "\n"))))))

(mylisp
 (let ((factorial (quote nil)))
   (set! factorial
     (lambda (x)
       (if (= x 0) 1
           (* x (factorial (- x 1))))))
   (display (factorial 10))))

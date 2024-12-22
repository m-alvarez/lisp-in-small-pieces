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

(struct env (bindings))

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

(define-syntax let-or
  (syntax-rules (let-or)
    ((let-or _err () body ...) (begin body ...))
    ((let-or err ((var val) bindings ...)
             body ...)
     (val (lambda (var) (let-or err (bindings ...) body ...)) err))))

(define ((ev-variable env expr) ok err)
  (if (symbol? expr)
      (let ((entry (lookup (env-bindings env) expr)))
        (if entry
            (ok (entry-get entry))
            (err (format "Unbound variable ~v" expr))))
      (err (format "Not a variable name: ~v" expr))))

(define ((ev-set! env var expr) ok err)
  (if (symbol? var)
      (let ((entry (lookup (env-bindings env) var)))
        (cond
          ((mpair? entry)
           (let-or err ((new-value (evaluate env expr)))
                   (set-mcdr! entry new-value)
                   (ok new-value)))
          ((cons? entry)
           (err (format "Cannot update mutable variable ~v" var)))
          (#t (err (format "Unbound variable ~v" var)))))
      (err (format "Not a variable name: ~v" expr))))


(define (literal? expr)
  (or (number? expr)
      (boolean? expr)
      (string? expr)
      (char? expr)
      (procedure? expr)))
(define ((ev-literal expr) ok _err) (ok expr))
(define ((ev-quote body) ok _err) (ok body))

(define (extend-env e vars vals)
  (define (extend-frame f vars vals)
    (cond
      ((and (null? vars) (null? vals)) f)
      ((or (null? vars) (null? vals))
       (error "INTERNAL ERROR"))
      (#t (extend-frame (cons (mcons (car vars) (car vals)) f) (cdr vars) (cdr vals)))))
  (env (cons (extend-frame '() vars vals) (env-bindings e))))

(define ((map-result fn args) ok err)
  (if (null? args) (ok '())
      (begin
        ((fn (car args))
         (lambda (x)
           (let-or err ((xs (map-result fn (cdr args))))
                   (ok (cons x xs))))
         err))))

(define ((ev-let env vars exprs body) ok err)
  (let-or err
          ((vals (map-result (lambda (expr) (evaluate env expr)) exprs))
           (blk (ev-block (extend-env env vars vals) body)))
          (ok blk)))

(define ((ev-if env cond tt ff) ok err)
  (let-or err ((cond-val (evaluate env cond)))
          (if (is-truthy cond-val)
              ((evaluate env tt) ok err)
              ((evaluate env ff) ok err))))

(define ((evaluate env expr) ok err)
  (cond
    ((symbol? expr)
     ((ev-variable env expr) ok err))
    ((literal? expr)
     ((ev-literal expr) ok err))
    ((list? expr)
     (match expr
       ((list 'quote expr)
        ((ev-quote expr) ok err))
       ((list 'let bindings body ...)
        (let* ((vars (map car bindings))
               (exprs (map cadr bindings)))
          ((ev-let env vars exprs body) ok err)))
       ((list 'if b tt ff)
        ((ev-if env b tt ff) ok err))
       ((list 'begin exprs ...)
        ((ev-block env exprs) ok err))
       ((list 'set! var val)
        ((ev-set! env var val) ok err))
       ((list 'lambda args body ...)
        ((ev-lambda env args body) ok err))
       ((list 'call/cc arg)
        ((ev-call/cc env arg) ok err))
       ((list 'catch catch
              body ...)
        ((ev-catch env catch body) ok err))
       ((list rator rands ...)
        ((ev-app env rator rands) ok err))))
    (#t
     (err (format "I don't know how to evaluate ~v" expr)))))

(define ((ev-catch env catch body) ok err)
  (let-or err ((catch (evaluate env catch)))
          ((ev-block env body)
           ok
           (lambda (msg)
             ((invoke env catch (list msg)) ok err)))))

(define ((ev-call/cc env arg) ok err)
  ((ev-app env arg (list ok)) ok err))

(define ((ev-block env exprs) ok err)
  (cond
    ((null? exprs) (ok '()))
    ((null? (cdr exprs))
     ((evaluate env (car exprs)) ok err))
    (#t
     (let-or err ((_ (evaluate env (car exprs))))
             ((ev-block env (cdr exprs)) ok err)))))

(define ((ev-lambda env args body) ok _err)
  (ok
   (list 'function env args body)))

(define ((ev-app env rator rands) ok err)
  (let-or err ((rator (evaluate env rator))
               (rands (map-result (lambda (e) (evaluate env e)) rands)))
          ((invoke env rator rands) ok err)))

(define ((invoke _env rator rands) ok err)
  (match rator
    (proc
     #:when (procedure? proc)
     (ok (apply proc rands)))
    ((list 'function closure-env args body)
     (if (= (length rands) (length args))
         ((ev-block (extend-env closure-env args rands) body) ok err)
         (err (format "Incorrect number of arguments for ~v" rator))))
    (_
     (err (format "I don't know how to invoke ~v" rator)))))


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

(define-syntax handle
  (syntax-rules (ok err)
    ((handle expr
             ((ok result) ok-body ...)
             ((err msg) err-body ...))
     (expr (lambda (result) (begin ok-body ...))
           (lambda (msg) (begin err-body ...))))))

(define-syntax mylisp
  (syntax-rules ()
    ((mylisp exprs ...)
     (handle (evaluate (env (list *GLOBALS*)) (quote (begin exprs ...)))
             ((ok result) (display result) (display "\n"))
             ((err msg) (display "ERROR: ") (display msg) (display "\n"))))))

(trace evaluate)
;(mylisp
; (let ((factorial (quote nil)))
;   (set! factorial
;         (lambda (x)
;           (if (= x 0) 1
;               (* x (factorial (- x 1))))))
;   (display (factorial 10))))
(mylisp
 (catch (lambda (err) (display "Found error") 666)
   x))

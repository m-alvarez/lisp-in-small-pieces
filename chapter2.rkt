#lang racket

(require racket/trace)
(require racket/format)

(define (massoc k l)
  (cond
    ((null? l) #f)
    ((and
      (mpair? (car l))
      (eq? k (mcar (car l))))
     (car l))
    (#t (massoc k (cdr l)))))

(define (mlookup env expr)
  (if (null? env)
      #f
      (let ((entry (massoc expr (car env))))
        (or entry (mlookup (cdr env) expr)))))

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
       (anything (error "INTERNAL ERROR IN LET?:" (quote expr)))))
    ((let? () body ...)
     (begin body ...))))

(struct context
  (lenv denv fenv))

; We Haskle now, boys
(define-syntax-rule (define-accessor name getter setter)
  (define (name obj)
    (lambda (update)
      (let ((new-value (update (getter obj))))
        (setter obj new-value)))))

(define (update-context-lenv c lenv)
  (context lenv (context-denv c) (context-fenv c)))
(define (update-context-denv c denv)
  (context (context-lenv c) denv (context-fenv c)))
(define (update-context-fenv c fenv)
  (context (context-lenv c) (context-denv c) fenv))
(define-accessor context-lenv> context-lenv update-context-lenv)
(define-accessor context-denv> context-denv update-context-denv)
(define-accessor context-fenv> context-fenv update-context-fenv)

(define (aget acc)
  (acc (lambda (x) x)))
(define (aset acc val)
  (acc (lambda (_) val)))
(define (apush acc val)
  (acc (lambda (l) (cons val l))))

(define (ev-variable env expr)
  (if (symbol? expr)
      (let ((entry (mlookup env expr)))
        (if entry
            (ev-ok (entry-get entry))
            (ev-err (format "Unbound variable ~v" expr))))
      (ev-err (format "Not a variable name: ~v" expr))))

(define (ev-set! ctx var expr)
  (let?  ((entry (match var
                   ((list 'dynamic v)
                    #:when (symbol? v)
                    (ev-ok (mlookup (context-denv ctx) v)))
                   ((list 'function v)
                    #:when (symbol? v)
                    (ev-ok (mlookup (context-fenv ctx v))))
                   (v
                    #:when (symbol? v)
                    (ev-ok (mlookup (context-lenv ctx) v)))
                   (#t
                    (ev-err (format "Not a variable name: ~v" expr))))))
         (if entry
             (let? ((new-value (evaluate ctx expr)))
                   (set-mcdr! entry new-value)
                   (ev-ok new-value))
             (ev-err (format "Unbound variable ~v" var)))))

(define (literal? expr)
  (or (number? expr)
      (boolean? expr)
      (string? expr)
      (char? expr)))
(define (ev-literal expr) (ev-ok expr))
(define (ev-quote body) (ev-ok body))

(define (extend-frame f vars vals)
  (cond
    ((and (null? vars) (null? vals)) f)
    ((or (null? vars) (null? vals))
     (error "INTERNAL ERROR"))
    (#t (extend-frame (cons (mcons (car vars) (car vals)) f) (cdr vars) (cdr vals)))))

(define (extend accessor vars vals)
  (apush accessor (extend-frame '() vars vals)))

(define (map-result fn args)
  (if (null? args) (ev-ok '())
      (match (fn (car args))
        ((cons 'ok val)
         (match (map-result fn (cdr args))
           ((cons 'ok vals) (ev-ok (cons val vals)))
           ((cons 'error err) (ev-err err))))
        ((cons 'error err) (ev-err err)))))

(define (ev-bind ctx acc vars exprs body)
  (let? ((vals (map-result (lambda (expr) (evaluate ctx expr)) exprs)))
        (ev-block (extend acc vars vals) body)))

(define (ev-if ctx cond tt ff)
  (let? ((cond-val (evaluate ctx cond)))
        (if (is-truthy cond-val)
            (evaluate ctx tt)
            (evaluate ctx ff))))

(define (evaluate ctx expr)
  (cond
    ((symbol? expr)
     (ev-variable (context-lenv ctx) expr))
    ((literal? expr)
     (ev-literal expr))
    ((list? expr)
     (match expr
       ((list 'quote expr)
        (ev-quote expr))
       ((list 'function expr)
        (ev-variable (context-fenv ctx) expr))
       ((list 'dynamic expr)
        (ev-variable (context-denv ctx) expr))
       ((list 'let bindings body ...)
        (let* ((vars (map car bindings))
               (exprs (map cadr bindings)))
          (ev-bind ctx (context-lenv> ctx) vars exprs body)))
       ((list 'dlet bindings body ...)
        (let* ((vars (map car bindings))
               (exprs (map cadr bindings)))
          (ev-bind ctx (context-denv> ctx) vars exprs body)))
       ((list 'flet bindings body ...)
        (let* ((vars (map car bindings))
               (exprs (map cadr bindings)))
          (ev-bind ctx (context-fenv> ctx) vars exprs body)))
       ((list 'if b tt ff)
        (ev-if ctx b tt ff))
       ((list 'begin exprs ...)
        (ev-block ctx exprs))
       ((list 'set! var val)
        (ev-set! ctx var val))
       ((list 'lambda args body ...)
        (ev-lambda ctx args body))
       ((list rator rands ...)
        (ev-app ctx rator rands))))))

(define (ev-block ctx exprs)
  (cond
    ((null? exprs) (ev-ok '()))
    ((null? (cdr exprs))
     (evaluate ctx (car exprs)))
    (#t (let? ((_ (evaluate ctx (car exprs))))
              (ev-block ctx (cdr exprs))))))

(define (ev-lambda ctx args body)
  (ev-ok
   (list 'function (context-lenv ctx) (context-fenv ctx) args body)))

(define (ev-app ctx rator rands)
  (let? ((rator (if (symbol? rator)
                    (ev-variable (context-fenv ctx) rator)
                    (evaluate ctx rator))))
        (match rator
          ((list 'function lenv fenv args body)
           (let? ((vals (map-result (lambda (expr) (evaluate ctx expr)) rands)))
                 (if (= (length vals) (length args))
                     (let ((lam-ctx (context lenv (context-denv ctx) fenv)))
                       (ev-block (extend (context-lenv> lam-ctx) args vals) body))
                     (ev-err (format "Incorrect number of arguments for ~v" rator)))))
          (proc
           #:when (procedure? proc)
           (let? ((vals (map-result (lambda (expr) (evaluate ctx expr)) rands)))
                 (ev-ok (apply rator vals))))
          (_ (ev-err (format "I don't know how to invoke ~v" rator))))))

(define-syntax push!
  (syntax-rules ()
    ((push! thing place)
     (set! place (cons thing place)))))
(define *GLOBALS* '())
(define-syntax def-primitive
  (syntax-rules ()
    ((def-primitive (name inputs ...) body ...)
     (push!
      (mcons (quote name)
             (let* ()
               (define (name inputs ...) body ...)
               name))
      *GLOBALS*))
    ((def-primitive name value)
     (push! (mcons (quote name) value) *GLOBALS*))))
(define-syntax import-primitive
  (syntax-rules ()
    ((import-primitive name)
     (push! (mcons (quote name) name) *GLOBALS*))))

;(def-primitive (cons x y) (mcons x y))
;(def-primitive (car l) (mcar l))
;(def-primitive (cdr l) (mcdr l))
;(def-primitive (set-car! l) (set-mcar! l))
;(def-primitive (set-cdr! l) (set-mcdr! l))
;(def-primitive (and x y) (and x y))
;(def-primitive (or x y) (or x y))
;(import-primitive eq?)
(import-primitive +)
(import-primitive -)
(import-primitive *)
;(import-primitive <)
(import-primitive =)
;(import-primitive not)
(import-primitive display)

(define-syntax my-lisp-2
  (syntax-rules ()
    ((my-lisp-2 expr)
     (match (evaluate (context (list) (list) (list *GLOBALS*)) (quote expr))
       ((cons 'ok result) result)
       ((cons 'error err) (display err) (display "\n"))))
    ((my-lisp-2 expr exprs ...)
     (match (evaluate (context (list) (list) (list *GLOBALS*)) (quote expr))
       ((cons 'ok _) (my-lisp-2 exprs ...))
       ((cons 'error err) (display err) (display "\n"))))))

(trace evaluate)
(my-lisp-2
 (let ((x 99))
   (flet ((double-x (lambda () (+ (dynamic x) x))))
         (dlet ((x 5))
               (display (double-x)))
         (dlet ((x 10))
               (display (double-x))))))

#lang racket

(define (massoc k1 l)
  (match l
    ((cons (mcons k2 v) ls)
     (if (eq? k1 k2) (car l)
         (massoc k1 ls)))
    ((list) #f)))
(provide massoc)

(define (ok result) (cons 'ok result))
(provide ok)
(define (err msg) (cons 'err msg))
(provide err)

(define-syntax let?
  (syntax-rules ()
    ((let? ((var expr) bindings ...) body ...)
     (match expr
       ((cons 'ok var)
        (let? (bindings ...) body ...))
       ((cons 'err msg) expr)
       (anything (error "INTERNAL ERROR IN LET?:" anything))))
    ((let? () body ...)
     (begin body ...))))
(provide let?)

(define (map? fn args)
  (if (null? args) (ok (list))
      (let? ((v (fn (car args)))
             (vs (map? fn (cdr args))))
            (ok (cons v vs)))))
(provide map?)

(define (self-quoting? atom)
  (or (number? atom)
      (boolean? atom
      (string? atom)
      (char? atom))))
(provide self-quoting?)

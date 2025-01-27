#lang racket

(require racket/trace)
(require racket/format)

(require "utils.rkt")


;; Here I am really hoping that LiSP won't need multiple dispatch

(define (lookup r var k)
  (match (massoc r var)
    ((mcons _ val) (resume k val))
    (_ (err (format "No such variable ~v" var)))))
(define (update! r var val k)
  (let ((entry (massoc r var)))
    (if entry
        (begin
          (set-mcdr! entry val)
          (resume k (list)))
        (err (format "No such variable ~v" var)))))

(define value%
  (class object%
    (define/public (invoke _rands _r _k)
      (error "Unimplemented"))))
(define (invoke rator rands r k)
  (send-generic (generic value% invoke) rator rands r k))

(define continuation%
  (class object%
    (define/public (resume _)
      (error "Unimplemented"))))
(define (resume cont v)
  (send-generic (generic value% invoke) cont v))

(define if-cont%
  (class continuation%
    (init when-true when-false environment continuation)
    (define tt when-true)
    (define ff when-false)
    (define r environment)
    (define k continuation)
    (super-new)

    (define/override (resume v)
      (evaluate (if v tt ff) r k))))
(define (ev-if c tt ff r k)
  (evaluate c r
            (new if-cont%
                 (when-true tt)
                 (when-false ff)
                 (environment r)
                 (continuation k))))

(define begin-cont%
  (class continuation%
    (init body environment continuation)
    (define b body)
    (define r environment)
    (define k continuation)
    (super-new)
    (define/override (resume _v)
      (ev-begin b r k))))
(define (ev-begin body r k)
  (match body
    ((list) (resume k (list)))
    ((list expr) (evaluate expr r k))
    ((list* expr body)
     (evaluate expr r (new begin-cont% (body body) (environment r) (continuation k))))))

(define set-cont%
  (class continuation%
    (init variable environment continuation)
    (define var variable)
    (define r environment)
    (define k continuation)
    (super-new)
    (define/override (resume val)
      (update! r var val k))))
(define (ev-set! var val r k)
  (evaluate val r (new set-cont% (variable var) (environment r) (continuation k))))

(define (ev-variable var r k)
  (lookup r var k))

(define (ev-apply rator rands r k) #f)

(define closure%
  (class value%
    (init parameters body environment)
    (define params parameters)
    (define b body)
    (define r environment)
    (super-new)
    (define/override (invoke rands r k)
      (error "todo"))))
      
(define (ev-lambda vars body r k) #f)

(define (ev-quote e _r k)
  (resume k e))

(define (evaluate e r k)
  (match e
    (sym #:when (symbol? sym)
         (ev-variable sym r k))
    (a #:when (self-quoting? a)
       (ev-quote a r k))
    ((list 'quote expr) (ev-quote expr r k))
    ((list 'if c tt ff) (ev-if c tt ff r k))
    ((list* 'begin body) (ev-begin body r k))
    ((list 'set! var val) (ev-set! var val r k))
    ((list* 'lambda args body) (ev-lambda args body r k))
    ((list* rator rands) (ev-apply rator rands r k))
    (_ (err (format "I don't know how to evaluate ~v" e)))))

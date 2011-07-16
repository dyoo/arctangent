#lang racket/base

(provide (all-defined-out))

(define-values (prop:setter
                prop:setter?
                prop:setter-accessor)
  (make-struct-type-property 'prop:setter))



;; Strings are a data structure that support the setter.
;; We also define a custom-write so that they still look like 
;; plain, vanilla strings.
(struct str (v)
  #:transparent
  
  #:property prop:procedure 
  (case-lambda 
    [(a-str index)
     (string-ref (str-v a-str) index)]
    [(a-str index v)
     (string-set! (str-v a-str) index v)])
  
  #:property prop:custom-write
  (lambda (a-str port mode)
    (let ([recur (case mode
                   [(#t) write]
                   [(#f) display]
                   [else (lambda (p port) (print p port mode))])])
      (recur (str-v a-str) port)))
  
  #:property prop:setter
  (lambda (a-str index v)
    (string-set! (str-v a-str) index v)))









;; The boolean values of this language
(struct arc-t ()
  #:transparent
  #:property prop:custom-write
  (lambda (_ port mode)
    (case mode
      [(#t) (display "t" port)]
      [(#f) (display "t" port)]
      [else (display "t" port)])))


(struct arc-nil ()
  #:transparent
  #:property prop:custom-write
  (lambda (_ port mode)
    (case mode
      [(#t) (display 'nil port)]
      [(#f) (display 'nil port)]
      [else (display 'nil port)])))

(define t (arc-t))
(define nil (arc-nil))


(define (list->arc-list lst)
  (cond
    [(null? lst)
     nil]
    [else
     (mcons (car lst)
            (list->arc-list (cdr lst)))]))



(define-syntax-rule (arc-true? x)
  (not (eq? x nil)))


(define-syntax-rule (arc-false? x)
  (eq? x nil))




(define-syntax-rule (adapt/bool x)
  (if x t nil))
  
(define (arc-odd x)
  (adapt/bool (odd? x)))

(define (arc-even x)
  (adapt/bool (even? x)))



(define (arc-is? x y)
  (if (and (str? x) (str? y))
      (string=? (str-v x) (str-v y))
      (eq? x y)))
#lang racket/base

(provide (all-defined-out))

(define-values (prop:setter
                prop:setter?
                prop:setter-accessor)
  (make-struct-type-property 'prop:setter))


;; Strings are a data structure that support the setter.
;; We also define a custom-write so that they still look like 
;; plain, vanilla strings.
(define-struct str (v)
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

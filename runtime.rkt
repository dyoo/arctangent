#lang racket/base

(provide (all-defined-out))

(define-values (prop:setter
                prop:setter?
                prop:setter-accessor)
  (make-struct-type-property 'prop:setter))


(define-struct str (v)
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
#lang racket/base

(require (for-template racket/base)
         (for-template "runtime.rkt"))

(provide convert-datum)

(define (convert-datum v for-quote?)
  (cond
    [(null? v)
     #'nil]
    [(pair? v)
     #`(arc-cons #,(convert-datum (car v) for-quote?)
                 #,(convert-datum (cdr v) for-quote?))]
    
    [(string? v)
     #`(str (string-copy #,v))]
    
    [else
     (cond
       [for-quote?
        #`(quote #,(datum->syntax #f v))]
       [else 
        #`(#%datum . #,(datum->syntax #f v))])]))

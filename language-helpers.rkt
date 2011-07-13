#lang racket/base

(require (for-template racket/base)
         (for-template "runtime.rkt"))

(provide convert-datum)

(define (convert-datum v for-quote?)
  (cond
    [(pair? v)
     #`(mcons #,(convert-datum (car v) for-quote?)
              #,(convert-datum (cdr v) for-quote?))]
    
    [(string? v)
     #`(make-str (string-copy #,v))]
    
    [else
     (cond
       [for-quote?
        #`(quote #,(datum->syntax #f v))]
       [else 
        #`(#%datum . #,(datum->syntax #f v))])]))
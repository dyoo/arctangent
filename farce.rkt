#lang racket/base

;; The absolute minimum necessary to play with:
;;
;; http://ycombinator.com/arc/tut.txt



(require (for-syntax racket/base)
         (for-syntax "runtime.rkt")
         "runtime.rkt")



(define-for-syntax (convert-datum v)
  (cond
    [(pair? v)
     (mcons (convert-datum (car v))
            (convert-datum (cdr v)))]
    
    [(string? v)
     (str v)]

    [else
     v]))


(define-syntax (my-datum stx)
  (syntax-case stx ()
    [(_ . v)
     (with-syntax ([converted (convert-datum (syntax->datum #'v))])
       #`(quote converted))]))



;; Variable assignment.
;; We expand the left hand side and see if it's already a bound identifier.
;; If it hasn't been bound yet, this becomes a definition.
;; Otherwise, it turns magically into an assignment.
(define-syntax (assign stx)
  (syntax-case stx ()
    
    [(_ lhs rhs)
     (let ([expanded-lhs (local-expand #'lhs 
                                       (syntax-local-context) 
                                       #f)])
       (begin 
         (cond
           [(syntax-property expanded-lhs 'setter)
            => (lambda (f)
                 (f #'rhs))]
           [(and (identifier? expanded-lhs)
                 (eq? #f (identifier-binding expanded-lhs)))
            (quasisyntax/loc stx
              (define #,expanded-lhs rhs))]
           [else
            (quasisyntax/loc stx
              (set! #,expanded-lhs rhs))])))]))



(define-syntax (my-quote stx)
  (syntax-case stx ()
    [(_ thing)
     (with-syntax ([converted (convert-datum (syntax->datum #'thing))])
       (syntax/loc stx 
         (quote converted)))]))

         
(define-syntax (my-car stx)
  (syntax-case stx ()
    [(_ v)
     (syntax-property (syntax/loc stx
                        (mcar v))
                      'setter
                      (lambda (rhs)
                        #`(set-mcar! v #,rhs)))]))

(define-syntax (my-cdr stx)
  (syntax-case stx ()
    [(_ v)
     (syntax-property (syntax/loc stx
                        (mcdr v))
                      'setter
                      (lambda (rhs)
                        #`(set-mcdr! v #,rhs)))]))


(define-syntax (def stx)
  (syntax-case stx ()
    [(_ name args body ...)
     (syntax/loc stx
       (define name (lambda args
                      body ...)))]))

(define-syntax (fn stx)
  (syntax-case stx ()
    [(_ args body ...)
     (syntax/loc stx
       (lambda args body ...))]))





(provide [rename-out [assign =]
                     [mcons cons]
                     [my-quote quote]
                     [my-car car]
                     [my-cdr cdr]
                     [my-datum #%datum]]
         #%top
         #%top-interaction
         #%module-begin
         #%app
         +
         /
         -
         *
         def
         fn)

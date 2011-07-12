#lang racket/base

;; The absolute minimum necessary to play with:
;;
;; http://ycombinator.com/arc/tut.txt



(require (for-syntax racket/base))


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


;; FIXME
;; We want to translate all the cons pairs in the syntax object to mpairs.
(define-syntax (my-quote stx)
  (syntax-case stx ()
    [(_ thing)
     #`(quote thing)]))

         
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






(provide [rename-out [assign =]
                     [mcons cons]
                     [my-quote quote]
                     [my-car car]
                     [my-cdr cdr]]
         #%top
         #%top-interaction
         #%datum
         #%module-begin
         #%app
         +)

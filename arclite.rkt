#lang racket/base

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
           [(eq? #f (identifier-binding expanded-lhs))
            (quasisyntax/loc stx
              (define #,expanded-lhs rhs))]
           [else
            (quasisyntax/loc stx
              (set! #,expanded-lhs rhs))])))]))

         
         


(provide [rename-out [assign =]]
         #%top
         #%top-interaction
         #%datum
         #%module-begin)

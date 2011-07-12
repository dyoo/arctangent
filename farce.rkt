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

         


(provide [rename-out [assign =]
                     [mcons cons]
                     [my-quote quote]]
         #%top
         #%top-interaction
         #%datum
         #%module-begin
         #%app
         +)


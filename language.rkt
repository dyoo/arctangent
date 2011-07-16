#lang racket/base

;; The absolute minimum necessary to play with:
;;
;; http://ycombinator.com/arc/tut.txt



(require (for-syntax racket/base)
         (for-syntax racket/list)
         (for-syntax "language-helpers.rkt")
         "runtime.rkt") 



(define-syntax (arc-datum stx)
  (syntax-case stx ()
    [(_ . v)
     (convert-datum (syntax->datum #'v) #f)]))



;; Returns true if stx is an identifier that's been lexically bound.
(define-for-syntax (lexically-bound? stx)
  (let ([expanded (local-expand stx (syntax-local-context) #f)])
    (cond
      [(and (identifier? expanded)
            (eq? #f (identifier-binding expanded)))
       #f]
      [else
       #t])))
     

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
           [(identifier? expanded-lhs)
            (cond
              [(eq? #f (identifier-binding expanded-lhs))
               (quasisyntax/loc stx
                 (define #,expanded-lhs rhs))]
              [else
               (quasisyntax/loc stx
                 (set! #,expanded-lhs rhs))])]
           [else
            (syntax-case expanded-lhs ()
              [(structure index)
               (quasisyntax/loc stx
                 (let ([data structure])
                   (if (prop:setter? data)
                       ((prop:setter-accessor data) data index rhs)
                       (error '= "~e does not support the setter protocol" data))))]
               
              [else
               (quasisyntax/loc stx
                 (set! #,expanded-lhs rhs))])])))]))



(define-syntax (arc-quote stx)
  (syntax-case stx ()
    [(_ thing)
     (convert-datum (syntax->datum #'thing) #t)]))

         
(define-syntax (arc-car stx)
  (syntax-case stx ()
    [(_ v)
     (syntax-property (syntax/loc stx
                        (mcar v))
                      'setter
                      (lambda (rhs)
                        #`(set-mcar! v #,rhs)))]))

(define-syntax (arc-cdr stx)
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
     (identifier? #'name)
     (cond 
       [(lexically-bound? #'name)
        (syntax/loc stx
          (set! name (lambda args
                         body ...)))]
       [else
        (syntax/loc stx
          (define name (lambda args
                         body ...)))])]))

(define-syntax (fn stx)
  (syntax-case stx ()
    [(_ args body ...)
     (syntax/loc stx
       (lambda args body ...))]))



(define-syntax (arc-let stx)
  (syntax-case stx ()
    [(_ name value body ...)
     (syntax/loc stx
       (let ([name value])
         body ...))]))


(define-for-syntax (evens lst)
  (cond
    [(empty? lst)
     '()]
    [else
     (cons (first lst)
           (evens (rest (rest lst))))]))


(define (pr . elts)
  (for-each display elts))


(define (prn . elts)
  (for-each display elts)
  (newline))

(define-for-syntax (odds lst)
  (cond
    [(empty? lst)
     '()]
    [else
     (cons (second lst)
           (odds (rest (rest lst))))]))



(define-syntax (arc-with stx)
  (syntax-case stx ()
    [(_ (name-value ...) body ...)
     (even? (length (syntax->list #'(name-value ...))))
     (with-syntax ([(name ...) (evens (syntax->list #'(name-value ...)))]
                   [(value ...) (odds (syntax->list #'(name-value ...)))])
       (syntax/loc stx
         (let ([name value] ...)
           body ...)))]))
                   


(define-syntax (arc-if stx)
  (syntax-case stx ()
    [(_ forms ...)
     (cond
       [(= (length (syntax->list #'(forms ...))) 0)
        (raise-syntax-error #f "missing clauses" stx)]
       [(= (length (syntax->list #'(forms ...))) 1)
        (raise-syntax-error #f "missing value" stx)]
       [else
        (let loop ([forms (syntax->list #'(forms ...))])
          (cond
            [(= (length forms) 0)
             (syntax/loc stx nil)]
            [(= (length forms) 1)
             (first forms)]
            [else
             (quasisyntax/loc stx
               (if (arc-true? #,(first forms))
                   #,(second forms)
                   #,(loop (rest (rest forms)))))]))])]))
            

(define-syntax (do stx)
  (syntax-case stx ()
    [(_ body ...)
     (syntax/loc stx
       (begin body ...))]))



(define-syntax (arc-when stx)
  (syntax-case stx ()
    [(_ test body ...)
     (syntax/loc stx
       (arc-if test
          (do body ...)))]))



(define-syntax (arc-and stx)
  (syntax-case stx ()
    [(_)
     (syntax/loc stx nil)]
    [(_ x)
     #'x]
    [(_ x y ...)
     (syntax/loc stx
       (arc-if x (arc-and y ...)))]))


(define-syntax (arc-or stx)
  (syntax-case stx ()
    [(_)
     (syntax/loc stx nil)]
    [(_ x)
     #'x]
    [(_ x y ...)
     (syntax/loc stx
       (let ([x-val x])
         (arc-if x-val
                 x-val
                 (arc-or y ...))))]))


(define (no x)
  (if (arc-true? x)
      nil
      t))




(provide [rename-out [assign =]
                     [mcons cons]
                     [arc-quote quote]
                     [arc-car car]
                     [arc-cdr cdr]
                     [arc-datum #%datum]
                     [arc-let let]
                     [arc-with with]
                     [arc-if if]
                     [arc-and and]
                     [arc-or or]
                     
                     [arc-odd odd]
                     [arc-even even]]
         #%top
         #%top-interaction
         #%module-begin
         #%app

         nil
         no
         
         def
         fn
         do

         +
         /
         -
         *
         sqrt
         expt
         
         pr
         prn
         )

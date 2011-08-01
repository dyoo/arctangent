#lang racket/base

;; The absolute minimum necessary to play with:
;;
;; http://ycombinator.com/arc/tut.txt



(require (for-syntax racket/base)
         (for-syntax racket/list)
         (for-syntax "language-helpers.rkt")
         racket/stxparam
         "runtime.rkt") 



(define-syntax-parameter arc-lambda-placeholder
  (lambda (stx)
    (raise-syntax-error #f "placeholder is being used outside of a function template" stx)))



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



(define-for-syntax (looks-like-composition? id)
  (let ([name (symbol->string (syntax-e id))])
    (let ([pieces (regexp-split #rx":" name)])
      (cond
        [(= (length pieces) 2)
         (let ([lhs (datum->syntax id (string->symbol (car pieces)))]
               [rhs (datum->syntax id (string->symbol (cadr pieces)))])
           (if (and (lexically-bound? lhs)
                    (lexically-bound? rhs))
               (list lhs rhs)
               #f))]
        [else
         #f]))))

(define-for-syntax (looks-like-negation? id)
  (let ([name (symbol->string (syntax-e id))])
    (let ([a-match (regexp-match #rx"~(.+)" name)])
      (cond
        [a-match
         (let ([maybe-negated-function                 
                (datum->syntax id (string->symbol (cadr a-match)))])
           (cond [(lexically-bound? maybe-negated-function)
                  maybe-negated-function]
                 [else
                  #f]))]
        [else
         #f]))))


;; When we hit on toplevel identifiers that we don't know about, see if
;; this is a use of the composition of two functions using ':', where the left
;; and right sides are bound identifiers.
(define-syntax (arc-top stx)
  (syntax-case stx ()
    [(_ . id)
     (cond
       [(looks-like-composition? #'id)
        => (lambda (lhs+rhs)
             (with-syntax ([lhs (car lhs+rhs)]
                           [rhs (cadr lhs+rhs)])
               (syntax/loc #'id 
                 (arc-compose lhs rhs))))]
       [(looks-like-negation? #'id)
        => (lambda (negated-function)
             (with-syntax ([negated-function negated-function])
               (syntax/loc #'id
                 (arc-negate negated-function))))]
       [else
        ;; Otherwise, just reuse Racket's #%top.
        (syntax/loc stx
          (#%top . id))])]))



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
                 (begin (define #,expanded-lhs rhs)
                        #,expanded-lhs))]
              [else
               (quasisyntax/loc stx
                 (begin (set! #,expanded-lhs rhs)
                        #,expanded-lhs))])]
           [else
            (syntax-case expanded-lhs ()
              [(structure index)
               (quasisyntax/loc stx
                 (let ([data structure]
                       [rhs-value rhs])
                   (if (prop:setter? data)
                       (begin ((prop:setter-accessor data) data index rhs-value)
                              rhs-value)
                       (error '= "~e does not support the setter protocol" data))))]
               
              [else
               (quasisyntax/loc stx
                 (let ([rhs-value rhs])
                   (set! #,expanded-lhs rhs-value)
                   rhs-value))])])))]))


(define-syntax (arc-zap stx)
  (syntax-case stx ()
    [(_ fn (structure index))
     (quasisyntax/loc stx
       (let ([data structure]
             [rhs-value (fn (structure index))])
         (if (prop:setter? data)
             (begin ((prop:setter-accessor data) data index rhs-value)
                    rhs-value)
             (error '= "~e does not support the setter protocol" data))))]
    [(_ fn id)
     (identifier? #'id)
     (quasisyntax/loc stx
       (assign id (fn id)))]))
    

(define-syntax (arc-increment stx)
  (syntax-case stx ()
    [(_ x)
     (syntax/loc stx
       (arc-zap add1 x))]))

(define-syntax (arc-decrement stx)
  (syntax-case stx ()
    [(_ x)
     (syntax/loc stx
       (arc-zap sub1 x))]))





(define-syntax (arc-quote stx)
  (syntax-case stx ()
    [(_ thing)
     (convert-datum (syntax->datum #'thing) #t)]))

         


(define-syntax (def stx)
  (syntax-case stx ()
    [(_ name args body ...)
     (identifier? #'name)
     (cond 
       [(lexically-bound? #'name)
        (syntax/loc stx
          (begin (set! name (fn args
                              body ...))
                 name))]
       [else
        (syntax/loc stx
          (begin (define name (fn args
                                body ...))
                 name))])]))

(define-syntax (fn stx)
  (syntax-case stx ()
    [(_ (id ...) body ...)
     (syntax/loc stx
       (lambda (id ...) body ...))]
    
    [(_ (id ... . rest-id) body ...)
     (with-syntax ([(rest-arg) (generate-temporaries #'(rest-id))])
       (syntax/loc stx
         (lambda (id ... . rest-arg) 
           (let ([rest-id (list->arc-list rest-arg)])
             body ...))))]))


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
            

(define-syntax (arc-do stx)
  (syntax-case stx ()
    [(_ body ...)
     (syntax/loc stx
       (begin body ...))]))



(define-syntax (arc-when stx)
  (syntax-case stx ()
    [(_ test body ...)
     (syntax/loc stx
       (arc-if test
          (arc-do body ...)))]))



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

(define-syntax (arc-case stx)
  (syntax-case stx ()
    [(_ val-exp k+v ...)
     (quasisyntax/loc stx
       (let ([val val-exp])
         #,(let loop ([k+vs (syntax->list #'(k+v ...))])
             (cond
              [(empty? k+vs)
               (syntax/loc stx nil)]
              [(empty? (rest k+vs))
               (first k+vs)]
              [else
               (quasisyntax/loc stx
                 (arc-if (arc-is val '#,(first k+vs))
                         #,(second k+vs)
                         #,(loop (rest (rest k+vs)))))]))))]))


(define (arc-list . args)
  (list->arc-list args))


(define (arc-is x y)
  (adapt/bool (arc-is? x y)))


(define (arc-iso x y)
  (adapt/bool (equal? x y)))


(define-for-syntax (contains-lambda-placeholder? los)
  (ormap (lambda (stx) (and (identifier? stx)
                            (free-identifier=? #'arc-lambda-placeholder stx)))
         los))


;; application sees if the expression is an implicit lambda
(define-syntax (arc-app stx)
  (syntax-case stx ()
    [(_ operator+operands ...)
     (cond
       [(contains-lambda-placeholder? (syntax->list #'(operator+operands ...)))
        (with-syntax ([(id) (generate-temporaries #'(_))])
          (syntax/loc stx
            (fn (id)
                (syntax-parameterize ([arc-lambda-placeholder (make-rename-transformer #'id)])
                                     (#%app operator+operands ...)))))]
       [else
        (syntax/loc stx
          (#%app operator+operands ...))])]
    [(_ . operator+operands)
     (syntax/loc stx
       (#%app . operator+operands))]
    [(_)
     (identifier? stx)
     (syntax/loc stx
       #%app)]))



     
    






(provide [rename-out [assign =]
                     [arc-cons cons]
                     [arc-quote quote]
                     [arc-car car]
                     [arc-cdr cdr]
                     [arc-list list]
                     [arc-datum #%datum]
                     [arc-let let]
                     [arc-with with]
                     [arc-if if]
                     [arc-and and]
                     [arc-or or]
                     [arc-is is]
                     [arc-iso iso]
                     [arc-odd odd]
                     [arc-even even]
                     [arc-top #%top]
                     [arc-lambda-placeholder _]
                     [arc-app #%app]
                     [arc-map map]
                     [arc-do do]
                     [arc-when when]
                     [arc-no no]
                     [arc-case case]
                     [arc-zap zap]
                     [arc-increment ++]
                     [arc-decrement --]]
         #%top-interaction
         #%module-begin

         nil
         
         def
         fn

         +
         /
         -
         *
         sqrt
         expt
         
         pr
         prn
         )

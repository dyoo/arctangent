#lang racket/base

(require racket/sandbox
         racket/runtime-path
         racket/list
         rackunit
         syntax/strip-context)

(define-runtime-path language.rkt "language.rkt")

(define my-eval #f)
(define output-port #f)
(define error-port #f)

(define (init-my-eval!)
  (set! output-port (open-output-string))
  (set! error-port (open-output-string))
  (parameterize ([sandbox-output output-port]
                 [sandbox-error-output error-port])
    (set! my-eval (make-evaluator `(file ,(path->string language.rkt))))))

(init-my-eval!)


(define (run-tests tests)
  (let loop ([last-result #f]
             [tests (syntax->list tests)])
    (cond
      [(empty? tests)
       (void)]
      [(eq? (syntax->datum (first tests)) '---)
       (init-my-eval!)
       (loop #f (rest tests))]
      [(eq? (syntax->datum (first tests)) '==>)
       (let ([src (first tests)]
             [expected (syntax->datum (second tests))])
         (with-check-info* (list (make-check-location (list (syntax-source src)
                                                            (syntax-line src)
                                                            (syntax-column src)
                                                            (syntax-position src)
                                                            (syntax-span src))))
                           (lambda ()
                             (check-equal? last-result expected))))
       (loop #f (rest (rest tests)))]
      [else
       (let ([next-result (my-eval (first tests))])
         (loop next-result (rest tests)))])))
      


(define program-tests
  (strip-context
   #'(42 ==> 42
         
      
      (= foo 13)
      foo   ==>   13
      'foo  ==>   foo
      
      ---
      
      (+ (+ 1 2) 
         (+ 3 (+ 4 5)))   ==> 15
      
      ---
      
      (cons 'f '(a b))    ==> (f a b)
      (= f '(a b))
      f                   ==> (a b)
            
      ---
      
      
      (= z (cons (cons 3 4) 5))
      (= (car (car z)) 'three)
      z                   ==> ((three . 4) . 5)
      
      (= (cdr z) 'five)
      z                   ==> ((three . 4) . five)
      
      ---
      
      (def average (x y)
        (/ (+ x y) 2))
      
      (average 2 4)       ==> 3
      
      ((fn (x y) (/ (+ x y) 2)) 2 4) ==> 3
      
      
      
      (= a (fn (x y) (/ (+ x y) 2)))
  
      (a 17 0) ==> 17/2
      
      "hello"   ==> "hello"
      ;("hello" 3)   ==> #\l
      ;("hello" 4)   ==> #\o
      ---

      
      (= message "Hello world")
      (= (message 2) #\x)
      message         ==> "Hexlo world"

      )))

  
(run-tests program-tests)
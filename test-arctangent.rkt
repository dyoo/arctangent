#lang racket/base

(require racket/sandbox
         racket/runtime-path
         racket/list
         rackunit
         syntax/strip-context)


(define-runtime-path language.rkt "language.rkt")
(define-runtime-path runtime.rkt "runtime.rkt")

(define my-eval #f)
(define output-port #f)
(define error-port #f)

(define my-namespace-specs
  (let ([specs (sandbox-namespace-specs)])
    `(,(car specs)
      ,@(cdr specs)
      racket/mpair
      (file ,(path->string runtime.rkt)))))


(define (init-my-eval!)
  (parameterize ([sandbox-output 'string]
                 [sandbox-namespace-specs my-namespace-specs])
    (set! my-eval (make-evaluator `(file ,(path->string language.rkt))))))


(define my-rack-eval 
  (parameterize ([sandbox-namespace-specs my-namespace-specs])
    (make-evaluator 'racket/base
                    #:requires `((file ,(path->string runtime.rkt))
                                 racket/mpair))))

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
             [expected (my-rack-eval (syntax->datum (second tests)))])
         (with-check-info* (list (make-check-location (list (syntax-source src)
                                                            (syntax-line src)
                                                            (syntax-column src)
                                                            (syntax-position src)
                                                            (syntax-span src))))
                           (lambda ()
                             (check-equal? last-result expected))))
       (loop #f (rest (rest tests)))]
      [(eq? (syntax->datum (first tests)) '==>/stdout)
       (let ([src (first tests)]
             [expected (my-rack-eval (syntax->datum (second tests)))])
         (with-check-info* (list (make-check-location (list (syntax-source src)
                                                            (syntax-line src)
                                                            (syntax-column src)
                                                            (syntax-position src)
                                                            (syntax-span src))))
                           (lambda ()
                             (check-equal? (get-output my-eval) expected))))
       (loop #f (rest (rest tests)))]
      
      [else
       (let ([next-result (my-eval (first tests))])
         (loop next-result (rest tests)))])))
      


(define program-tests
  (strip-context
   #'(42 ==> 42
         
      
      (= foo 13)
      foo   ==>   13
      'foo  ==>   'foo
      
      ---
      
      (+ (+ 1 2) 
         (+ 3 (+ 4 5)))   ==> 15
      
      ---
      
      (cons 'f '(a b))    ==> (list->arc-list '(f a b))
      (= f '(a b))
      f                   ==> (list->arc-list '(a b))
            
      ---
      
      
      (= z (cons (cons 3 4) 5))
      (= (car (car z)) 'three)
      z                   ==>  (mcons (mcons 'three  4)  5)
      
      (= (cdr z) 'five)
      z                   ==> (mcons (mcons 'three  4)  'five)
      
      ---
      
      (def average (x y)
        (/ (+ x y) 2))
      
      (average 2 4)       ==> 3
      
      ((fn (x y) (/ (+ x y) 2)) 2 4) ==> 3
      
      
      
      (= a (fn (x y) (/ (+ x y) 2)))
  
      (a 17 0) ==> 17/2
      
      "hello"   ==> (str "hello")
      ("hello" 3)   ==> #\l
      ("hello" 4)   ==> #\o
      ---

      
      (= message "Hello world")
      (= (message 2) #\x)
      message         ==> (str "Hexlo world")

      ---
      
      (let x 1
        (+ x (* x 2))) ==> 3
      
                       
      (with (x 3 y 4)
            (sqrt (+ (expt x 2)
                     (expt y 2)))) ==> 5
       
      ---
                                   
      (prn "hello")  ==>/stdout '"hello\n"
      
      ---
      
      (def average (x y)
        (prn "my arguments were: " x " " y)
        (/ (+ x y) 2))
      
      (average 100 200)
      ==> 150
      ==>/stdout '"my arguments were: 100 200\n"
      
      
      ---
      
      (if (odd 1) 'a 'b) ==> 'a
      (if (odd 2) 'a 'b) ==> 'b
      
      
      ---
      
      (odd 1) ==> t
      (odd 2) ==> nil
      
      ---
      
      (if (odd 2) 'a) ==> nil
      
      ---
      
      (do (prn "hello")
        (+ 2 3))
      ==> 5
      ==>/stdout '"hello\n"
      
      ---
      
      (and nil (pr "you'll never see this"))
      ==> nil
      ==>/stdout '""
      
      ---
      
      (def mylen (xs)
       (if (no xs)
           0
           (+ 1 (mylen (cdr xs)))))

      (mylen nil)
      ==> 0
      
      (mylen '(a b))
      ==> 2
      
      ---
      
      ;; Re-definition is allowed.
      (def f (x) (* x x))
      (f 3) ==> 9
      (def f (x) (* x x x))
      (f 3) ==> 27

      ---
      
      (is 'a 'a) ==> t
      (is 'a 'b) ==> nil
      (is "foo" "foo") ==> t
      (iso "foo" "bar") ==> nil
      (let x (list 'a) (is x x)) ==> t
      (is (list 'a) (list 'a)) ==> nil
      
      ---

      (iso 'a 'a) ==> t
      (iso 'a 'b) ==> nil
      (iso "foo" "foo") ==> t
      (iso "foo" "bar") ==> nil
      (let x (list 'a) (iso x x)) ==> t
      (iso (list 'a) (list 'a)) ==> t
      (iso (list 'a) (list 'b)) ==> nil

      
      ---
      (def foo (x y . z) 
       (list x y z))
      
      (foo (+ 1 2) (+ 3 4) (+ 5 6) (+ 7 8)) ==> (mcons 3 (mcons 7 (mcons (mcons 11 (mcons 15 nil))
                                                                         nil)))


      ---
      
      
      )))

  
(run-tests program-tests)
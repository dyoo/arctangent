#lang s-exp "farce.rkt"

(= foo 13)
foo
'foo



(+ (+ 1 2) (+ 3 (+ 4 5)))

(cons 'f '(a b))
(= f '(a b))



(= z (cons (cons 3 4) 5))
(= (car (car z)) 'three)
z

(= (cdr z) 'five)
z
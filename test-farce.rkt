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

(def average (x y)
  (/ (+ x y) 2))

average

(fn (x y) (/ (+ x y) 2))


(= a (fn (x y) (/ (+ x y) 2)))

((fn (x y) (/ (+ x y) 2)) 2 4)

"hello"
("hello" 3)

(= message "Hello world")
;(= (message 2) #\x)
#lang s-exp syntax/module-reader
(planet dyoo/arctangent/language)



;; During compilation, tell the system that we're allowing set! from outside a module.
(compile-enforce-module-constants #f)

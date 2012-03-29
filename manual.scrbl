#lang scribble/manual

@(require racket/sandbox
          syntax/strip-context
          scribble/eval)

@title{Arctangent: an approach to language hacking}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]


@(define arc-eval
   (call-with-trusted-sandbox-configuration 
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string]
                     [print-as-expression #f])
        (define eval (make-evaluator "language.rkt"))
        ;; Kludge kludge kludge
        ;; Something is wrong with @interaction, so we have to kludge
        ;; our way around it...
        (define (wrapped-eval x)
          (cond [(equal? x '(map [+ _ 10] '(1 2 3)))
                 ;; This is the first example that demonstrates anonymous lambdas
                 ;; via brackets.
                 (eval (strip-context #'(map [+ _ 10] '(1 2 3))))]
                [else
                 (eval x)]))
        wrapped-eval))))


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@; Happy April Fools Day 2012.
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


@section{Introduction}

Back in 2008, Paul Graham and Robert Morris released a language called
@link["http://arclanguage.org"]{Arc}.  Their stated goal was to design
a language for exploratory programming.  The language looks much like
a Lisp, but with several features to make the language briefer and
more implicit, with the expectation that good programmers can take
advantage of brevity and avoid shooting themselves in the foot.

Their implementation builds on top of the
@link["http://racket-lang.org"]{Racket} language, using Racket's
existing runtime and
@link["http://docs.racket-lang.org/guide/reflection.html"]{dynamic
evaluation} features to bootstrap the implementation.  Arc's official
implementation, however, doesn't reuse Racket's module system to
organize programs, nor Racket's hygienic macro system to build the
language's semantics.

As a consequence of this, the rest of the Racket language toolchain
(like the @link["http://docs.racket-lang.org/raco/make.html"]{bytecode
compiler} or @link["http://docs.racket-lang.org/raco/exe.html"]{binary
package generator}) can't be easily reused by the Arc community.
Similarly, the Racket community can't reuse the work that the Arc
community has put into their libraries.  The situation is awkward.

@; I'd like to put "Arc-ward" here instead, but oh well.


I believe that a core reason for this is so that the Arc creators can
make a principled, minimalist approach toward language development.
They want the @emph{kernel} of their language to use as few primitives
as possible.  It's likely that the Arc developers excluded the use of
Racket's built-in macro and module systems because the developers
considered those features too conceptually large to be considered
kernel.

But what if we take an unprincipled approach?

Toward that end, the following is a primer on how to use Racket's
language infrastructure to hack together a Arc-like language.  This
tutorial is targetted toward programmers who have some intermediate
Racket experience, but should be otherwise self-contained.  Like the
@link["http://hashcollision.org/brainfudge"]{brainf*ck} tutorial,
we'll focus on how to define the semantics of a language with macros
and modules.

The language that we'll develop here is deliberately named
@bold{Arctangent} because it's only tangentially concerned with Arc.
The real purpose of this tutorial is to show the gearing in Racket's
language toolchain.  We'll deviate from the Arc language when it helps
to simplify the tutorial's presentation.

With that, let's begin!



@subsection{A brief look at Arctangent}

@margin-note{You can try this yourself, by writing a module in
 @litchar{#lang planet dyoo/arctangent}.}
Before we start, let's quickly approach Arctangent and see what it looks like.

Every programming language almost always has numbers, strings, and
pairs as primitives, as does Arctangent:

@interaction[#:eval arc-eval
25
"foo"
(cons 'hi nil)
]

We can bind names to variables, or rebind them:
@interaction[#:eval arc-eval
(= answer 41)
(= answer (+ answer 1))]

A computer language should have functions to define and apply:
@interaction[#:eval arc-eval
(def average (x y)
  (/ (+ x y) 2))
(average 3 4)
]

Respectively, strings also act like functions... but can also be mutated:
@interaction[#:eval arc-eval
(= a-string "hello again")
(a-string 0)
(a-string 1)
(= (a-string 1) #\i)
a-string
]

Even parentheses can be functions... by using square brackets:
@interaction[#:eval arc-eval
             (map [+ _ 10] '(1 2 3))
]

So beware!  Arctangent has several features that certainly aren't
@litchar{#lang racket}.




@subsection{Setting up a PLaneT link}

Now that we have an better idea of what Arctangent is about, let's see
how to implement it.

What we first want to do is tell Racket that we're defining a new
language, and that its definition lives somewhere on our filesystem.





@subsection{Overview of the rest of the document}

We'll build Arctangent iteratively, by starting off with the empty
language, and then build it up progressively.


@section{Bare bones}

(Minimal language with no bindings except top-interaction)

@subsection{Making a @filepath{lang/reader.rkt}}



@section{Simple data}

Let's first expose simple literal data constants into the language:
without these, we can't even talk about strings or numbers.

@subsection{Booleans}
@subsection{Strings, numbers}



@section{A primer for Racket's macro system}

@subsection{Compile-time computation vs run-time computation}

@subsection{Languages with Macros}

... discuss the toplevel macros @racket[#%app], @racket[#%top],
@racket[#%datum], etc ...

@subsection{Defining a @racket[let]}
@subsection{Defining a @racket[with]}




@section{Variables}
@subsection{Binding variables}
@subsection{Better interaction with the REPL and @racket[=]}



@section{Setters}
@subsection{Setting variables}
@subsection{Revisiting strings}
@subsection{Generalizing setters}


@section{More data}
@subsection{Pairs}
@subsection{Customizing printing behavior of values}





@section{Functions}
@subsection{Defining and using functions}





@section{Weirder tricks}
@subsection{Function composition}
@subsection{Function negation}
@subsection{Block-bracketed implicit lambdas}

We can implement these features by hacking the @racket[#%top] and
@racket[#%app] language macros, but let's take a slightly different
approach.  The features are really defined syntactically rather than
semantically, so let's reflect that by changing our
@filepath{reader.rkt} instead.
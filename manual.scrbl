#lang scribble/manual

@title{Arctangent: a unprincipled glance at language hackery}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]


@section{Introduction}

Back in 2008, Paul Graham and Robert Morris released a language called
@link["http://arclanguage.org"]{Arc}.  Their stated goal was to design
a language for exploratory programming.  The language looks much like
a Lisp, but with several features to make the language briefer and
more implicit, with the expectation that good programmers can be more
expressive and still avoid shooting themselves in the foot.

Their implementation builds on top of the
@link["http://racket-lang.org"]{Racket} language, using Racket's
existing runtime and
@link["http://docs.racket-lang.org/guide/reflection.html"]{dynamic
evaluation} features to bootstrap the implementation.  Arc's official
implementation, however, doesn't reuse much else of Racket's support
for language construction: it doesn't use Racket's modules to organize
programs, nor Racket's hygienic macro system to build the language's
semantics.

As a consequence of this, the rest of the Racket language toolchain
(like the @link["http://docs.racket-lang.org/raco/make.html"]{bytecode
compiler} or @link["http://docs.racket-lang.org/raco/exe.html"]{binary
package generator}) can't be easily reused by the Arc community.
Similarly, the Racket community can't reuse the excellent work that
the Arc community has put into their libraries.

I believe that a core reason for this re-invention is so that the Arc
creators can take a principled, minimalist approach toward language
development.  They want the kernel of their language to use as few
primitives as possible.  As a result, it is likely that they excluded
the use of most of the Racket toolchain (macros, modules) because it
was too conceptually large to be considered part of their kernel.

But what would things look like if we didn't have such morals?  What
if we take an unprincipled approach?

Toward that end, the following is a primer on how to use Racket's
language infrastructure to hack together an Arc-like language.  This
tutorial is targetted toward programmers who have some intermediate
Racket experience, but should be self contained.  Unlike the
@link["http://hashcollision.org/brainfudge"]{brainf*ck} tutorial, we
won't be so focused on syntactic issues, but rather how to define the
semantics of a language.  The language that we'll develop here is
deliberately named @emph{Arctangent} because we don't intend to
implement Arc.  That way, we'll be free to deviate from the language
if it helps to demonstrate certain features of the Racket language
toolchain or simplify its presentation.




@subsection{A brief look at Arctangent}




@subsection{Setting up a PLaneT link}

Before we do so, let's set up a PLaneT link...





@subsection{Overview of the rest of the document}

We'll build this language iteratively, by starting off with the empty
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
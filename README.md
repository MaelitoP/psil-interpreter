Psil is a small statically typed functional language with a Lisp-style syntax — the name is "Lisp" spelled backwards. This repository is an interpreter for it, written in Haskell.

The interpreter runs as a small pipeline. A Parsec reader turns source text into s-expressions, those are lowered into a typed core language, a bidirectional type checker validates each top-level expression, and an evaluator reduces it to a value. The language has integers and booleans, curried first-class functions, `let` bindings, tuples with destructuring, conditionals, and explicit type annotations.

I built it to learn Haskell on something more substantial than exercises, and to see how an interpreter fits together end to end. Writing the reader, the lowering to a core language, the type checker, and the evaluator as distinct stages made the usual textbook pipeline concrete — in particular the split between type synthesis and checking, and the way currying falls out of a core with single-argument functions.

`sample.psil` shows the surface syntax; `tests.psil` collects longer expressions. Functions are applied with an explicit `(call f x)` form, and every top-level expression is printed back as `value : type`.

Running it needs GHC and cabal. Build the package, then run a file through the interpreter:

    cabal build
    cabal run psil -- sample.psil

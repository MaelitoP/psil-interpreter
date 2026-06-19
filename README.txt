Psil is a small statically typed functional language with a Lisp-style syntax. The name is "Lisp" spelled backwards. This repository is an interpreter for it, written in Haskell.

The interpreter runs as a small pipeline. A Parsec reader turns source text into s-expressions, those are lowered into a typed core language, a bidirectional type checker validates each top-level expression, and an evaluator reduces it to a value. The language has integers and booleans, curried first-class functions, `let` bindings, tuples with destructuring, conditionals, and explicit type annotations.

I built it to learn Haskell on something more substantial than exercises, and to see how an interpreter fits together end to end. Writing the reader, the lowering to a core language, the type checker, and the evaluator as distinct stages made the usual textbook pipeline concrete.

Running it needs GHC and cabal. Build the package, then run a file through the interpreter:

    cabal build
    cabal run psil -- sample.psil

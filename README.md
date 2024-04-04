# YALC
This is an untyped lambda calculus interpreter written in Haskell. It is currently not finished. Most likely, it will be either using Parsec or Megaparsec. It will do beta reductions and alpha conversions.

I may consider adding a typing system or more advanced features later on. I don't know yet. There is a repl, which implies state variables might be added at some point. 

## Syntax

This interpreter uses the lambda (λ) symbol. It does not accept other symbols.
An example is shown with the Church booleans:
`(λp.λq.p p q)(λx.λy.x)(λx.λy.y)` will output as `λx.λy.x`
``

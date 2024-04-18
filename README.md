# YALC
This is an untyped lambda calculus interpreter written in Haskell. It is currently not finished. Most likely, it will be either using Parsec or Megaparsec. It will do beta reductions and alpha conversions.

I may consider adding a typing system or more advanced features later on. I don't know yet. There is a repl, which implies state variables might be added at some point. 

## Example Syntax

```
# macros directly substituted
# let is a reserved word (variable cannot be named let)
let and = λp.λq.p q p
let true = λx.λy.x
let false = λx.λy.y

(and) (true) (false)
5
# outputs (λp.λq.p p q)(λx.λy.x)(λx.λy.y) --> λx.λy.x
```

Integer literals:
```
32

# outputs λf.λx.f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(fx)))))))))))))))))))))))))))))))
```
## Basic Data Structure
```haskell
type Label = [Char]
data Lambda = Var Label | Apply Lambda Lambda | Lam Label Lambda
type LambdaState = Map Label Lambda -- represents macros

```

# YALC
This is an untyped lambda calculus interpreter written in Haskell. Most likely, it will be either using Parsec or Megaparsec. It will do beta reductions and alpha conversions.

I may consider adding a typing system or more advanced features later on. I don't know yet. There is a repl, which implies state variables might be added at some point. 

## Installation

Haskell dependency management is a mess. I was originally considering just creating a nix file to automatically setup the proper environments, but there were too many issues with that. Some libraries also have external FFI-based dependencies that are not automatically installed. That said, here is the environment I have used. It may work on other systems as well, but results may vary:
OS: Arch Linux
Stack: 2.15.5
Cabal: 3.10.3.0
GHC: 9.8.2 (base 4.19.1.0)

Then, it should be able to open into REPL mode with `cabal run`. The code isn't guarenteed to be "production grade", because of my extremely limited time to work on the project, I did not get to do much rigorous testing.

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

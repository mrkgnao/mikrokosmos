---
date: 2018-01-06
title: Learning the Bound library
---

I've been interested in Edward Kmett's [Bound][bound] library for a while. Here
are some notes that I'm taking as I explore it, primarily using the example
from the Bound module, with a few examples using --- surprise ---

~~~ {.haskell-eval}
unwords ["real", "doctest-y", "code", "blocks!"]
~~~

This file is literate Haskell. 

> {-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell #-}
> import Bound
> import Control.Applicative
> import Control.Monad (ap)
> import Data.Functor.Classes
> import Data.Foldable
> import Data.Traversable
> import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1) 

Our language
===
 
We'll work with a simple untyped lambda-calculus:

> infixl 9 :@
> data Exp a 

We have variables,

>   = V a 

function applications,

>   | Exp a :@ Exp a 

and lambdas:

>   | Lam (Scope () Exp a)
>   deriving (Functor, Foldable, Traversable)

but wait, what's `Scope`? 

A `Scope b f a` is used to express a construct, like a `let` or a lambda-abstraction, 
that brings bindings into scope. (Often just called a "binder".) 

And where's the variable the lambda binds? 

de Bruijn notation
===

Bound uses a variation on de Bruijn notation, which is a "nameless" way to
represent, e.g. lambda-calculus terms. For instance, the function

> f :: ((a -> a) -> b) -> b
> f = \z -> (\y -> y (\x -> x)) (\x -> z x)

(which is just an obfuscated way of writing `f z = z id`!) can be written in
1-based de Bruijn notation as

    λ (λ 1 (λ 1)) (λ 2 1)

![](/images/de-bruijn.png)

What's happening here, as you may have realised, is that an occurrence of a
natural number `n` refers to the \\(n\\)th lambda away from it. So, now using
0-based de Bruijn notation,

< id  = \x -> x         = λ 0
< ($) = \f -> \x -> f x = λ λ (1 0)

What does Bound give me?
==

Let's define a couple smart constructors using some of the combinators `Bound`
gives us for free.

> lam :: Eq a => a -> Exp a -> Exp a
> lam v b = Lam (abstract1 v b)
> 
> -- | Weak-head normal form.
> whnf :: Exp a -> Exp a
> whnf (f :@ a) = case whnf f of
>   Lam b -> whnf (instantiate1 a b)
>   f'    -> f' :@ a
> whnf e = e

Now let's look at our examples from before. We'll write them using `lam`:

~~~ {.haskell-eval}
lam 'x' (V 'x')
~~~

Here `B` stands for "bound", as opposed to `F`, which stands for "free":

~~~ {.haskell-eval}
lam 'x' (V 'y')
~~~

How do we go "up" the chain of binders? 

~~~ {.haskell-eval}
lam 'x' (lam 'y' (V 'x')) -- const!
~~~

It seems one can think of `B` as \\({\\sf zero} = 0\\) and `F` as \\({\\sf succ}\\) in, e.g.

< data Nat = Zero | Succ Nat

Each application of `F` allows us to push everything one level deeper. It seems important that
this can be done without actually traversing the AST. All we have to do is wrap it in another 
layer of \\(\\sf succ\\): Kmett highlights the fact that this gets us a lot of time-complexity 
improvements. \\(\\sf succ\\) in \\(O(1)\\)!

Instances
===

> instance Applicative Exp where 
>   pure  = V
>   (<*>) = ap
>
> instance Monad Exp where
>   return = V
>   V a      >>= f = f a
>   (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
>   Lam e    >>= f = Lam (e >>>= f)
> 
> deriveEq1   ''Exp
> deriveOrd1  ''Exp
> deriveRead1 ''Exp
> deriveShow1 ''Exp
> 
> instance Eq a   => Eq   (Exp a) where (==) = eq1
> instance Ord a  => Ord  (Exp a) where compare = compare1
> instance Show a => Show (Exp a) where showsPrec = showsPrec1
> instance Read a => Read (Exp a) where readsPrec = readsPrec1

[bound]: https://hackage.haskell.org/package/bound

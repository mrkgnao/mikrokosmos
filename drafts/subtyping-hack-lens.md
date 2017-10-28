---
title: The "subtyping hack" for van Laarhoven optics
date: 2017-10-27T21:00:00Z
---

**This is a work in progress.**

### Preamble

This post is a literate Haskell file, and can be executed or loaded into `GHCi`
with `-pgmL markdown-unlit`. We cast a few protective spells

```haskell
{-# LANGUAGE RankNTypes, TypeOperators #-}
```

and summon the benevolent spirits whose aid we need.

```haskell
import Data.Constraint
import Data.Functor
import Control.Applicative
```

# Constraint implications

Haskell's syntax for constrained typeclass instances is kind of the wrong way around
if one reads `=>` as implication. Observe:

```hs
class Functor f => Applicative f where ...
```

As a young novitiate who had barely learned to parse Haskell syntax, I
distinctly remember being confused for a brief moment: `f` being an instance of
`Functor` does not imply it is an `Applicative`! 

In fact, exactly the opposite holds. Since the declaration of the class says
any instances are expected to also bring `Functor` instances with them,
`Applicative f` implies `Functor f`! (This is one of the many small warts in
Haskell that PureScript fixes.) 

## Aside: proving constraints

One can prove this with Kmett's `constraints`
package, which exposes an implication operator `(:-)`:

```haskell
proof :: Applicative f :- Functor f
proof = Sub Dict
```

The converse does not hold and will fail to typecheck.

# The `lens` subtyping trick

The key to how a bunch of `lens` functions that demand a `Traversal` work fine
when a lens is supplied to them comes from the implication we just proved
above: specifically, it hinges on how being `Applicative` implies you're a
`Functor`.

**A lens is a function of a certain shape polymorphic over a functor.**
**A traversal is the same thing, but polymorphic over an _applicative_.**

In other words, `Lens` is more polymorphic (since not all functors are
applicatives, it has to contend with a larger number of possible "inputs"), and
hence more restrictive: `Lens s t a b` is a *subtype* of `Traversal s t a b`.

```hs
type Lens      s t a b = forall f. Functor     f => (a -> f b) -> s -> f t 
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t 
```

A lens can't use anything outside of what `Functor` provides, but `Traversal`s
can use `pure` and `(<*>)` and so on in their implementations.

## The dictionary-passing transform

Another way to see this, which may be easier for those of you who know a
Java-like language, is through function subtyping (which is where the ["get-put
rule"][get-put] comes from). 

To do this, we consider the class instances as *arguments*.  This isn't too
wrongheaded: internally, GHC applies a *dictionary-passing transform* that
converts classes to simple datatypes and instances to values, since Core (which
is System F with coercions) doesn't have typeclasses. 

For instance, one can imagine something equivalent to

```hs
-- this is the class
data Monoid a = MonoidDict { mempty :: a, mappend :: a -> a -> a }

-- this is the instance
monoid_String :: Monoid String
monoid_String = MonoidDict "" (++)

-- and this is a function that uses Monoid instances
-- ordinary_mconcat :: Monoid a => [a] -> a
transformed_mconcat :: Monoid a -> [a] -> a
transformed_mconcat dict = foldl' (mappend dict) (mempty dict)
```

happening somewhere deep in the bowels of the compilation pipeline. 

You can always pass subtypes to functions, since a subtype supports all the
"operations" the required type would have. This is similar to how one can pass
`5 :: Int` to `(+ 2) :: Num a => a -> a`.

The slogan here[^void] is that "function subtyping is contravariant in the
arguments and covariant in the result", which explains why lenses are valid
`Traversals`.

[get-put]: https://www.ibm.com/developerworks/library/j-jtp07018/index.html
[^void]: This is another way to talk about positive and negative position, as
  Michael Snoyman does in [To Void or to
  void](https://www.fpcomplete.com/blog/2017/07/to-void-or-to-void). Asking for
  a value of type `Void` is an impossible-to-fulfil constraint, since it has no
  non-bottom inhabitants, and also no subtypes but itself. 

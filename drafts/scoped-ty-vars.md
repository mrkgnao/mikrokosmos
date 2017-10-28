---
title: Scoped type variables
date: 2017-10-28
---

Haskell's subtyping is of a decidedly different nature, though. Haskell doesn't
have classes or inheritance, but polymorphism can do interesting things.

Consider the following definitions:

```haskell
data A = {- unimportant -}

takingInt :: (forall a. Int -> a) -> A
givingInt :: (forall a. a -> Int) -> A
```

and a couple of identity functions:

```haskell
idGen :: a -> a
idGen x = x

idInt :: Int -> Int
idInt x = x
```

Here's a question: which of the following function calls will type-check?

1. `takingInt idGen`
2. `takingInt idInt`
3. `givingInt idGen`
4. `givingInt idInt`


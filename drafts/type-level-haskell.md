---
title: "Type-level Haskell 1: Polymorphism"
date: 2017-10-30
---

# What this is about

Polymorphism, at least etymologically, is the ability for one thing to take on,
or present itself in, multiple shapes. 

Within the discipline of statically typed programming, in languages like Java,
C++, or Haskell, it refers to data structures or functions that have the
ability to operate upon values of different *types* without having to duplicate
code or completely give up the types altogether.  (One can do the latter using
C's `void *` or Go's `interface{}` for code that behaves like this, at the
expense of type-safety.) This often goes by the name of "generics" in Java or
"templating" in C++.

Here's an example of a polymorphic Haskell function.

```hs
if_then_else :: Bool -> a -> a -> a
if_then_else True  x y = x
if_then_else False x y = y
```

The `a` is a *type variable*: a placeholder that can be *instantiated to* any
other type, provided we instantiate it to the same type **every time**. For
instance, one might call `if_then_else` with two `Char` values, like so:

```hs
>>> if_then_else True 'a' 'b'
'a'

>>> if_then_else False 'a' 'b'
'b'
```

In these invocations, the `a` type variable has been instantiated to `Char`, so
we have effectively used the function as if it had the type

```haskell
if_then_else :: Bool -> Char -> Char -> Char
```

\\(asdf\\)


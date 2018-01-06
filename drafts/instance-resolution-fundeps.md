---
title: "Typeclass-fu: how instance resolution works"
date: 2017-11-09
---

# Preamble

```haskell
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
```

If you're following along in GHCi, you'll have to additionally do

```
<λ> :set -XFlexibleContexts -XNoExtendedDefaultRules
```

# Trying, failing, learning

Consider the following hypothetical typeclass:

```haskell
class Convert a b where
  convert :: a -> b
```

This defines a function `convert` which will, given a value of type `a`,
produce a value of type `b` *if an instance of the form `Convert a b` exists*.

I'm not so sure.

Observe:

```haskell
instance Convert Char Int where
  convert ch = fromEnum ch
```

Now there is exactly one instance of `Convert` in existence, and it says that
we can convert a `Char` to an `Int` using the `fromEnum` function:

```haskell
<λ> fromEnum 'a'
97
```

But look what happens when we try to use it:

```
<λ> convert 'a'

<interactive>:18:1: error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘print’
      prevents the constraint ‘(Show a0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      These potential instances exist:
        instance Show Ordering -- Defined in ‘GHC.Show’
        instance Show Integer -- Defined in ‘GHC.Show’
        instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
        ...plus 22 others
        ...plus 13 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In a stmt of an interactive GHCi command: print it
```

That's weird: what GHCi is telling us is that it couldn't figure out a type. Let's investigate: what does it think the type of the expression we printed is?

```hs
<λ> :t convert 'a'
convert 'a' :: Convert Char b => b
```

Huh. `convert 'a'` has a type that translates to "any `b` that `Char` can be converted to". But doesn't that imply `b ~ Int`?

```haskell
```

```haskell
```

```haskell
```

```haskell
```

---
title: "Field guide to category theory, episode I"
subtitle: "In glorious Technicolor, with both math and Haskell examples."
blurb: "In glorious Technicolor, with both math and Haskell examples."
date: 2017-06-17
tags: [category-theory]
categories: [math.CT]
---

>  “The introduction of the cipher 0 or the group concept was general nonsense too, and mathematics was more or less stagnating for thousands of years because nobody was around to take such childish steps…”
>  <footer>Alexander Grothendieck</footer>

The notion of a category is one of the most useful concepts ever discovered in mathematics. 

Like most good mathematics, it is *fundamental*: categories and functors underlie a large part of modern mathematical practice. 

It is *powerful*: categorical techniques have been used to radically reimagine existing methods and expand the limits of their abilities.

It is *versatile*: a running example will be the usefulness of categorical abstractions in a situation far removed from academic mathematics, namely, practical, efficient functional programming as exemplified by the language Haskell. 

And, like a lot of good mathematics, it lends itself to *clarifying* existing ideas beautifully in a way that is useful for both the student (who can build on existing knowledge to understand new ideas) and the expert (who can draw analogies between seemingly unrelated concepts).

----

The definitions themselves are often "childish", as Grothendieck put it. The idea of category is slightly similar to that of a *graph*, but a closer analogy might be to a transit map for a subway system. There are stations, and a bunch of ways to go from one to another, and (as long as you're not pressed for time) these paths from one place to another are the same in that you'll eventually get there. A category is defined similarly: there are things, and ways to move between them. Formally, a category $\rc$ is given by

1. a collection $\ob \rc$ of *objects*
2. for $X, Y \in \ob\rc$, a collection $\cmor \rc X Y$ of *morphisms* from $X$ to $Y$
3. for $X, Y, Z \in \ob\rc$, a *composition* map $\cmoc X Y \times \cmoc Y Z \to \cmoc X Z$

which satisfy the following:

4. for all $X\in\ob\rc$, there is a unique two-sided identity in $\cmoc X X$ for the composition map
5. for all $X,Y,Z$, the composition map is *associative*

For elements $X \in \ob\rc$, we will usually write $X \in \rc$. $\cmor \rc X Y$ may also be denoted $\mor \rc X Y$ to ease the notational burden when working with multiple categories. Elements $f \in \moc X Y$ will usually be denoted $f : X \to Y$, suppressing any reference to $\rc$. For $f:X\to Y$, $X$ is the *source* and $Y$ is the *target* of $f$.

The composition of $f : X \to Y$ and $g : Y \to Z$ will generally be written in *diagrammatic* order: the morphism  $g \circ f$ will be written $f \triangleright g : X \to Z$ (or even $fg$). The $\triangleright$ operator signifies the composition map in property (3), and its use is meant to guide the eye along the "flow" of the morphisms. With these things cleared up, we can complete the definition of category.

An *identity morphism* is a map $\id A : A \to A$ that acts as a *two-sided identity* under composition. That is, given $ f : X \to Y $ and $g:Y\to X$, 

\[ \begin{align}
\id X&\trr f &= f\\
g &\trr \id X &= g
\end{align}\]

Morphisms satisfying the first property are *left identities for composition*, and those satisfying the second are *right identities*, whence the phrase *two-sided identity*. (One-sided identities will be useful later, and the notion is a useful one to understand.) Usually, an identity morphism for $X$ is one that does "nothing", often sending each piece of $X$ to itself.

$$\begin{tikzcd}
    K \arrow[r, hook] & L \\
    A \arrow[u] \arrow[r] & B \arrow[u]
  \end{tikzcd}
$$

*Associativity* for $\trr$ is the property that 

$$f \trr (g \trr h) = (f \trr g) \trr h$$

for all suitable choices of $f, g, h$. 

The phrase "suitable choice" above is one of the first of many examples of handwavy mathematical jargon, and it is worthwhile to work through understanding it. To unpack it, we first ask ourselves the stupid question: are any choices unsuitable? Composition is defined as 

$$ \trr : \moc X Y \times \moc Y Z \to \moc X Z$$

and, in this case, unsuitable choices are those where the definition of
composition does not let us form $ f \trr g $. One possible such choice is $ f
: X \to Y, g : X \to Y $: here $ f\trr g$ is meaningless, since the target of
$f$ does not match the source of $g$. For composition to be associative, all
choices of $f,g,h$ for which composition can be performed should satisfy the
property given above.

## Examples of categories

The category of sets, denoted $\set$, is defined by setting $\ob \set$ to be
the collection of all sets, and $\mor\set X Y$ to be the collection of
functions $f : X \to Y$. Composition is then the familiar composition of
functions: $ (f \trr g)(x) = g(f(x)) $.

The category of Haskell types, denoted ${\sf Hask}$, has Haskell types as
objects (e.g. `Int`, `Text`, `IO (Maybe a)`) and functions `a -> b` as
morphisms from `a` to `b`. Composition can then be defined as 

```haskell
-- | asdf
(|>) :: (a -> b) -> (b -> c) -> a -> c
f |> g = \x -> g (f x)
```

which the reader may recognize as being another spelling of `(>>>)` from
`Control.Arrow`. The `(|>)` operator is just the standard `(.)` operator from
the Prelude (which corresponds to $\circ$), but with the order of the arguments
reversed: `(|>) = flip (.)` is also a valid definition. The identity morphisms
are all described by

```haskell
id :: a -> a
id x = x
```

which is a polymorphic function that works for any `a`.


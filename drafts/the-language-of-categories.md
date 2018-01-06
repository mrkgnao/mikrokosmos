---
title: Monopost
date: 2017-11-26
---

* categories
* functors
* examples

# Localisation

Let $A$ be a ring. Consider a *multiplicative* subset of $A$: a subset $S
\subset A$ closed under multiplication, such that $1 = 1_R \in S$. (This is
actually equivalent to asking that $S$ be a *monoid* -- specifically, a
submonoid of the additive monoid of $A$ -- but this is just language, though.)
We would like to construct a ring where the elements of $S$ are invertible. It
turns out that the monoid condition is precisely what we need to ensure that
such a construction is possible and gives rise to a ring.

There is, of course, an explicit construction that we will detail below, but
here is a simple example of a ring you may have heard of that can be
constructed in this way. Define the **dyadic rationals** as the subring of $\mathbf
Q$ comprising fractions $x = p/q$ such that $q = 2^n$ for some positive integer
$n$ when $x$ is in lowest terms. (Some examples: $1/8$, $5/16$, $-7/2$.)

We can define an equivalence relation on $A \times S$ as follows:

$$(a, s) \sim (a', s') \iff r(s' a - s a') = 0 \text { for some } r \in S$$

Let's show that that this is an equivalence relation. Reflexivity and symmetry
are immediate, and might lead one to wonder what the $r$ is doing in the
definition.  It is when proving *transitivity* that the need for it becomes
clear.

Suppose $(a_1, s_1) \sim (a_2, s_2) \sim (a_3, s_3)$: that is, there are $p$
and $q$ in $S$ such that

$$p(a_1 s_2 - a_2 s_1) = 0$$

$$q(a_2 s_3 - a_3 s_2) = 0$$

Then we want an $r$ such that

$$r(a_1 s_3 - a_3 s_1) = 0$$

You can check that $r = pq a_2$ works:

$$
pqa_2(a_1 s_3 - a_3 s_1) = qa_3 \cdot p(a_1 s_2 - a_2 s_1) + pa_1 \cdot
q(a_2 s_3 - a_3 s_2) = 0
$$

We define the *localisation of $A$ at $S$* as a ring whose underlying set is

$$S^{-1}A = (A \times S) / \sim$$

where one writes the class of the element $(a, s)$ as $a/s$. Then, under the
operations

$$\frac as + \frac bt := \frac{at + bs}{st}$$

$$\frac as \cdot \frac bt := \frac {ab} {st}$$

it is easy to check that $S^{-1} A$ is a ring.

## The map to the localisation

There is a map of rings $A \to S^{-1} A$, sending $a$ to $a/1$.

**If $0 \in S$, the ring $S^{-1}A$ is $0$.**

Indeed, for any two fractions $a/s$ and $b/t$, we need $r$ such that $r(at -
bs) = 0$. But $r = 0$ always works!

**This map is injective iff $S$ contains no zerodivisors.**

One direction is immediate: if $S$ contains no zerodivisors, then given $a/1 =
b/1$, we have

$$r (a - b) = 0$$

and we can cancel the $r$ to get $a = b$.

In the other direction, suppose the map $A \to S^{-1}A$ is injective: that is,
$a/1 = b/1$ implies $a = b$. Then

$$(\exists r.\, r(a-b) = 0) \implies a - b = 0$$

which is exactly the statement that, given $r\in S$ and $s \in A$, $rs = 0$
implies $s = 0$. (TODO: what if $0 \in S$?)

**$A \to S^{-1}A$ satisfies a universal property**

Any $A$-algebra $\alpha : A \to B$ that sends $S$ to invertible elements of $B$ (that is, $\alpha(S) \subseteq B^\times$) factors through the $A$-algebra $i_S : A \to S^{-1}A$.

We want a map $\bar\alpha : S^{-1}A \to B$ such that

$$\alpha(a) = \bar\alpha (i_S (a)) = \bar\alpha(a / 1)$$

It is easy to construct a map that satisfies this, but since we also require uniqueness, it is better to derive the map directly from this property, the definition of a ring homomorphism, and the hypothesis that $\alpha(S) \subseteq B^\times$.

Observe that $\bar\alpha(a/s) = \bar\alpha(a/1) \cdot \bar\alpha(1/s) = \alpha(a) \cdot \bar\alpha(1/s)$. Since $\alpha(s) = \bar\alpha(s / 1)$ is invertible in $B$, and $\bar\alpha(1/s) \cdot \bar\alpha(s/1) = \bar\alpha(1) = 1$, we can write

$$\bar\alpha(1/s) = {\bar\alpha(s/1)}^{-1} = {\alpha(s)}^{-1}$$

and, finally, $$\bar\alpha(a/s) = \alpha(a) \cdot {\alpha(s)}^{-1}$$

**$S^{-1}A$-modules versus $A$-modules**

Let $M$ be an $S^{-1}A$-module (and hence an $A$-module). Then it is trivially shown that, for any $s : S$, $$\mu_s := s \bullet - : M \to M$$ is an isomorphism in ${\sf{Mod}}_A$, with an inverse given by the map $\mu_{1/s}$.

Now suppose $\mu_s : M \to M$ is known to be an isomorphism of $A$-modules. Denoting actions with $\bullet$, we define $(1/s) \underset{S^{-1}A}\bullet m = \mu_s^{-1}(m)$. Then

$$(a/s) \underset{S^{-1}A}\bullet m = (1/s) \underset{S^{-1}A} \bullet (a \underset A\bullet m) = \mu_s^{-1} (a \underset A\bullet m)$$

defines an $S^{-1}A$-module structure on $M$.

# Assignments

Monday, September 25: 1.1-1.3 (in the notes)

* Introduction to the class
* Some category theory abstracting what you know well
* Categories, functors, universal properties
* Posets=partially ordered sets
* Localization, tensor products.

Wednesday, September 27: 1.3-1.4

* Fibered products, products, coproducts, monomorphisms, Yoneda’s lemma
* Limits and colimits of diagrams in a category
* All limits in the category of sets exist
* Filtered colimits of sets exist
* All colimits of A-modules exist.

Friday, September 29: 1.4-1.5

* Adjoints
* Key examples (1.5.D and 1.5.D)
* Groupification of abelian semigroups.

Monday, October 2: 2.1-2.2

* Presheaves and sheaves
* Germs of a sheaf at a point
* The stalk of a sheaf at a point.

Wednesday, October 4: 2.2-2.3

* Examples of sheaves: restriction of sheaves, (locally) constant sheaves, morphisms to X sections of a map, pushdforward
* Morphisms of (pre)sheaves.

Friday, October 6: 1.6, 2.3-2.4, beginning of 1.6

* Properties determined at the level of stalks
* Compatible germs, and sheafification.

Monday, October 9:  rest of 1.6

* Abelian categories.

Wednesday, October 11:  2.5, 2.7, 2.6

* Brief introduction to the inverse image sheaf.

Friday, October 13:  2.6, beginning of 3.1

* The inverse image (different points of view)
* Beginning to think about schemes
* The underlying set of an affine scheme
* (Examples next day!)  Problem set 1 due.

Monday, October 16: 3.1-3.2

* Examples of the underlying set of the spectra of various rings
* Statement of the Nullstellensatz (and Zariski’s Lemma)
* Quotients and localizations induce subsets of Spec’s.

Wednesday, October 18:  3.2-3.5

* Maps of rings induce maps of Spec’s as sets
* Functions are not determined by their values at points, and the reason is nilpotents
* The Zariski topology (on Spec A), and Vanishing set V(.)
* A base for the Zariski topology on Spec A:  the Doesn’t-vanish sets D(f).

Friday, October 20 (taught by Sean Howe):  3.5-3.6

* Problem set 2 due
* Functions on Spec A
* Picturing ${\mathbb{A}}^2_{\mathbb{C}}$:   closed points, generic points, open sets, closed sets, principal open sets
* The topology on Spec A
* Connectedness, irreducibility.

Monday, October 23 (taught by Sean Howe):  3.6-3.7

* Noetherian topological spaces, decomposition into irreducible components, Noetherian induction, Noetherian rings and modules, Quasicompactness, closed points, specialization, generisation, generic point of a closed set
* I(S), the bijection between radical ideals and closed subsets, prime ideals and irreducible closed subsets, minimal prime ideals and irreducible components.

Wednesday, October 25 (taught by Brian Conrad):  4.1-4.2

* The structure sheaf on the distinguished base of Spec A
* The structure sheaf.

Friday, October 27 (taught by Brian Conrad):  4.3

* Isomorphism of ringed spaces, affine scheme, scheme, functions on open subsets of a scheme, Zariski topology on scheme
* Problem set 3 due.

Monday, October 30:  4.3-4.4

* Locally ringed space, residue field at a point \kappa(p), the line with the doubled origin, the projective line.

Wednesday, November 1:  4.4-4.5

* Projective space
* Graded rings, and the Proj construction
* Projective A-schemes.

Friday, November 3:  5.1-5.2

* Topological properties:  connected, connected component, (ir)reducible, irreducible component, quasicompact, generization/specialization, generic point, Noetherian topological space, closed point
* Quasiseparated, reduced, integral scheme; function field
* Introduction to the Affine Communication Lemma.

Monday, November 6:  5.3

* Affine Communication Lemma
* A-schemes, (locally) Noetherian schemes, finite type A-schemes
* Degree of a closed point of a finite type k-scheme
* (Quasi)affine and (quasi)projective varieties.

Wednesday, November 8:  5.4, 6.1, 6.2

* Normality and factoriality
* Various motivations for what morphisms of “geometric spaces” should be
* Morphisms of ringed spaces
* Morphisms of locally ringed spaces, and schemes.

Friday, November 10:  6.3-6.4

* The category of A-schemes
* Many examples of discussing morphisms without excess cutting-into-affines
* Some maps to projective space
* Maps of graded rings, and maps of projective schemes
* Problem set 4 due.

Monday, November 13:  6.5

* Rational maps (from reduced schemes):  rational maps, dominant, birational rational maps
* For irreducible affine varieties, dominant rational maps are “the same as” inclusions of function fields in the opposite direction.

Wednesday, November 15 (taught by Pablo Solis):  7.1-7.2

* Metrics for being “reasonable”: local on target, stable under composition, stable under base change
* E.g
* open embeddings
* Affine morphisms, integral homomorphisms, and the adjugate matrix trick.

Friday, November 17 (taught by Pablo Solis):  7.2-7.3

* First glimpses why base change is important
* Integral ring maps, Nakayama
* Finite morphisms
* (Ravi says:  skip the finite presentation discussion, as it is mangled.)  Problem set 5 due.

# Problem sets

1. (due Friday October 13). Do 10 of the following problems from the June 4, 2017 version of the notes: 

* 1.2.B, 1.3.A, 1.3.D, 1.3.E, 1.3.F, 1.3.I, 1.3.N, 1.3.O, 1.3.P, 1.3.R, 1.3.V, 1.4.B, 1.4.E, 1.4.F, 1.5.E, 1.5.F, 1.5.G, 1.5.H, 2.2.A, 2.2.I, 2.3.A, 2.3.B
* required to be done by the end of problem set 5:  1.3.C, 1.3.G (if you haven’t seen tensor products before), 1.3.H, 1.3.Q, 1.3.S, 1.3.X, 1.3.Y, 1.4.A, 1.4.C, 1.5.C, 2.2.G, 2.2.J, 2.3.C,
* only if you have the background, or want to learn about something:  1.2.D, 1.3.Z.

2. (due Friday October 20):  Do 10 of the following problems from the June 4, 2017 version of the notes:

* 2.3.I, 2.3.J, 2.4.E, 2.4.F, 2.4.I, 2.4.J, 2.4.K, 2.4.L, 2.4.M, 2.4.N, 2.4.P, 2.5.A, 2.5.B, 2.5.C, 2.5.D, 2.5.E, 2.5.F, 2.5.G, 2.5.H, 2.6.C, 2.6.D, 2.6.E, 2.6.F, 2.7.D.
* any of the earlier required problems not yet done.
* required to be done by the end of problem set 5:  2.3.E, 2.4.A, 2.4.C, 2.4.D, 2.6.B, 2.7.A, 2.7.B, 2.7.C.
* only if you have the background, or want to learn about something:  3.1.A, 3.1.B.

3. (due Friday October 30):  Do 10 of the following problems from the June 4, 2017 version of the notes:

* any of the earlier required problems not yet done (note that I’ve changed the instructions for them, so you just need to finish them by the end of problem set 5)
* 3.2.A, 3.2.C, 3.2.D, 3.2.E, 3.2.G, 3.2.H, 3.2.I, 3.2.L, 3.2.M, 3.2.N, 3.2.Q, 3.2.R, 3.2.S, 3.2.T, 3.4.C, 3.4.D, 3.4.E, 3.4.F, 3.4.H, 3.4.K.
* required to be done if you haven’t seen them before:  3.2.J, 3.2.K, 3.2.J, 3.2.K.
* required to be done by the end of problem set 5:  3.2.O or 3.2.P;  3.4.I, 3.4.J.

4. (due Friday November 10):  Read all of the problems, and be familiar with their contents.  Do 10 of the following problems from the June 4, 2017 version of the notes:

* any of the earlier required problems not yet done
* 3.5.B, 3.5.C, 3.5.E, 3.6.B, 3.6.E, 3.6.G, 3.6.I, 3.6.J, 3.6,K, 3.6.O, 3.6.R (or 3.6.U), 3.6.S, 3.6.T, 3.7.D, 3.7.E, 3.7.F, 3.7.G, 4.1.A, 4.1.B, 4.1.D, 4.3.A, 4.3.B, 4.3.F (required), 4.3.G (required), 4.4.A, 4.4.D, 4.4.F, 4.5.A, 4.5.C, 4.5.D, 4.5.E, 4.5.I, 4.5.K

5. (due Friday November 17):  Read all of the problems, and be familiar with their contents.  Do 10 of the following problems from the June 4, 2017 version of the notes:

* any of the earlier required problems not yet done
* 4.5.O, 5.1.B, 5.1.E, 5.1.F, 5.1.I, 5.2.A, 5.2.C, 5.2.E, 5.2.F, 5.2.H, 5.2.I, 5.3.A, 5.3.B, 5.3.C, 5.3.E, 5.4.I, 5.4.J, 6.2.A, 6.2.C, 6.2.D, 6.3.B, 6.3.C, 6.3.E, 6.3.J, 6.3.I, 6.3.M, 6.3.N, 6.4.D, 6.4.E, 6.4.G
* challenge problems:  5.1.H, 5.4.M
* required to be done if you haven’t seen them before:  5.4.A, 5.4.F
* required to be done by the end of Problem set 6:  5.4.H, 6.3.F, 6.4.A

Problem set 6 (due Friday December 1):  Read all of the problems, and be familiar with their contents.  Do 10 of the following problems from the November 18, 2017 version of the notes (which are probably the same):

* any of the earlier required problems not yet done
* 7.2.A, 7.2.B, 7.2.C, 7.2.I, 7.3.A, 7.3.B, 7.3.C, 7.3.D, 7.3.F, 7.3.G, 7.3.H, 7.3.J, 7.3.K, 7.3.L, 7.3.M.
* required to be done:  7.1.B.
* required to be done if you haven’t seen them before:  7.2.F, 7.2.G, 7.2.H.


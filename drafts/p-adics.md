---
title: Constructing the field of \(p\)-adic numbers
subtitle: "\\(p\\)-adic numbers are elements of a very interesting kind of field that one can obtain from \\(\\mathbf Q\\). They have fascinating properties, and are very useful in number theory."
blurb: A short, decidedly talky introduction to the \(p\)-adics, with a couple of constructions.
tags: [number-theory, valuation-theory, abstract-algebra, math.NT]
date: 2016-12-23
---

<div style="display: none">
\\[ 
  \\DeclareMathOperator{Mod}{mod} 
  \\DeclareMathOperator{Frac}{Frac} 
\\]
</div>

## Meta: historical notes

On a previous edition of this blog, in another life[^adele], I wrote a post on \\(p\\)-adic numbers (in July 2015) and [posted it](https://www.reddit.com/r/math/comments/3c6f52/i_learned_a_little_about_padics_recently_and/) to /r/math on Reddit. (That blog has long ceased to exist.) In September, a Redditor apparently found that old post (I have no idea how), and messaged me about whether he could find the post anywhere. I said it'd be a "few weeks", but, well, whatever.

I found a copy on the Internet Archive and copied the text of it (since there was no way to recover the original Markdown, to the best of my knowledge) and, after a lot of regex-replacing in Spacemacs, managed to fix it up (as I promised in the [first post on this blog](/in-which-we-talk-a-lot/)) enough that I can post it again.

While \\(p\\)-adics are still not something I'd say I'm completely comfortable with, the fact that I can now construct them in multiple ways and can think of the constructions at higher levels of abstraction (e.g. "quotient field of a limit") has improved my ability to think about concrete examples. It's fascinating to go through this and see how my thinking has evolved in the short span of a bit over a year. For this reason, I've left it mostly unchanged, besides fixing a few typos here and there, and adding two "interludes" (on metric completions and fraction fields, respectively) that were missing in the original.

*End meta.*

Throughout this piece, \\(p\\) is a prime.

## Representing a number as a sequence of remainders

Say you wish to find a solution to the equation \\(x ^ 2+1=0\\) in \\(\{\\mathbf{Z}}\\). (Of course, there aren’t any, but just bear with me.)

It often happens that considering equations modulo some number (not necessarily a prime) tells us a lot about them. This happens because of the following fact: if an equation has integer solutions, these solutions will also be valid if we consider both sides mod any number, so if the equation has no solutions modulo a certain number \\(n\\) (i.e. in \\(\{\\mathbf{Z}}/n\{\\mathbf{Z}}\\)), it has no solutions in \\(\{\\mathbf{Z}}\\).

For instance, the following is an easy exercise (hint: consider both sides mod \\(4\\)).

<blockquote>
<span style="font-variant: small-caps;">Exercise</span>.<br>
Show that \\(x ^ 2+y^2=435\\) has no solutions for integer \\(x\\) and \\(y\\).
</blockquote>

Now, as we consider things modulo bigger and bigger numbers, we sort of "widen" the range of numbers we’re looking at. This inspires the following construction: instead of considering numbers modulo a fixed prime, we can look at their remainders mod every power of the prime. So, for instance, \\(25978\\) is

| modulus              | representative |
|----------------------|----------------|
| \\(\\Mod 3 ^ 1\\)    | \\(1\\)        |
| \\(\\Mod 3 ^ 2 \\)   | \\(4\\)        |
| \\(\\Mod 3 ^ 3\\)    | \\(58\\)       |
| \\(\\Mod 3 ^ 4\\)    | \\(220\\)      |
| \\(\\Mod 3 ^ 5\\)    | \\(463\\)      |
| \\(\\Mod 3 ^ 6\\)    | \\(1921\\)     |
| \\(\\Mod 3 ^ 7\\)    | \\(6295\\)     |
| \\(\\Mod 3 ^ 8\\)    | \\(6295\\)     |
| \\(\\Mod 3 ^ 9\\)    | \\(25978\\)    |
| \\(\\Mod 3 ^ {10}\\) | \\(25978\\)    |
| \\(\\Mod 3 ^ {11}\\) | \\(25978\\)    |
 
After \\(3 ^ {10}\\), the powers of \\(3\\) are greater than 25978, so the remainders are just \\(25978\\) itself, and the series "bottoms out". It is easy to see that this must be the case for every positive integer.

This suggests that we represent an integer \\(n\\) by a sequence of numbers

\\[(a _ 0,a _ 1,a _ 2,a _ 3,\\ldots)\\]

where \\(a _ k\\) is the remainder of \\(n\\) modulo \\(p ^ {k+1}\\). (Equivalently, \\(a _ {i+1} \\Mod {p^{i+1}}=a _ i\\).) For instance, \\(25978\\) can be represented by the infinite tuple \\((1,4,4,58,\\ldots)\\) and so on.

<blockquote>
<span style="font-variant: small-caps;">Exercise</span>.<br>
Check the <em>equivalently</em> bit.
</blockquote>

A little work shows that \\(2\\) is a solution to \\(x ^ 2+1=0\\) in \\(\{\\mathbf{Z}}/5\{\\mathbf{Z}}\\). Passing to \\(\{\\mathbf{Z}}/25\{\\mathbf{Z}}\\), we see that \\(7\\) is a solution in this ring. Continuing this, we can say that the infinite sequence \\((2,7,57,182,\\ldots)\\) is a "solution" to \\(x^2+1=0\\) "in the \\(5\\)-adics".

## Introducing \\(\{\\mathbf{Z}} _ p\\)

However, what if the remainders didn’t bottom out? What if there were something – obviously not an integer, as we’ve seen – that kept on giving different remainders modulo higher and higher powers of a prime? In much the same way as before, such an object can easily be represented as

\\[(a _ 0,a _ 1,a _ 2,a _ 3,\\ldots)\\]

that is, an infinite sequence of remainders, with the relation

\\[a _ {i+1} \\Mod {p ^ {i+1}}=a _ i.\\]

This kind of thing is called a \\(p\\)-adic integer. (Of course, technically, so are garden variety integers, but don’t nitpick.)

If you’re fancy, you can now define the ring[^lim] of \\(p\\)-adics as follows:

\\[\{\\mathbf{Z}} _ p=\\{(a _ 0,a _ 1,a _ 2,\\ldots)∈\\prod\{\\mathbf{Z}}/p ^ k\{\\mathbf{Z}}:a _ {i+1} \\Mod {p^{i+1}} = a _ i\\} \\]

<blockquote>
<span style="font-variant: small-caps;">Exercise</span>. <p class="fullwidth">Check that termwise addition and multiplication make \\(\{\\mathbf{Z}} _ p\\) into a (commutative) ring.</p>
</blockquote>

Next, we arrive at \\(\{\\mathbf{Z}} _ p\\) from the analytical point of view.

## A few definitions

Quickly, a metric space is a set A together with a metric \\(d:A×A→R\\), such that:

* *positive definiteness*: \\(d(x,y)=0 \\iff x=y\\)
* *symmetry*: \\(d(x,y)=d(y,x)\\)
* the *triangle inequality*: \\(d(x,z)\\leq d(x,y)+d(y,z)\\)

all hold. The commonest examples include \\(\\mathbf R ^ n\\) (where we use the norm of the difference between two vectors) and rings like \\(\{\\mathbf{Z}}\\), \\(\{\\mathbf{Q}}\\), and \\(\{\\mathbf{C}}\\) (where the metrics are the standard "absolute values" of the difference between two elements).

### An interesting example

Consider the space of functions

\\[\\mathscr C([0,1],\{\\mathbf{R}})=\\{\\text{continuous } f:[0,1]→\{\\mathbf{R}} \\}\\]

where the metric is

\\[d(f,g)=\\sup _ {x\\in[0,1]}|f(x)−g(x)|\\]

<div class="bd-callout bd-callout-info">
<h4>Exercise</h4>
<p>
Prove that \\(\\mathscr C([0,1],\{\\mathbf{R}})\\) is a metric space.
</p>
</div>

## The \\(p\\)-adic metric

Define

\\[p ^ n\\lVert x\\]

for \\(p\\) a prime if \\(p ^ n\\) is the highest power of \\(p\\) dividing \\(x\\).
We define the symbol

\\[\\nu _ p(x)=n\\]

where \\(p ^ n \\lVert x\\). This is called a \\(p\\)-adic valuation.

Now define \\(d _ p:\\mathbf{Z\\times Z\\to R}\\) by

\\[d _ p (x,y)=p ^ {−\\nu _ p(x−y)}\\]

which essentially says that two numbers are ‘close’ in some sense if their difference is divisible by a large power of \\(p\\).

<div class="bd-callout bd-callout-info">
<h4>Notational convention</h4>
<p>
We write \\(\\|x,y\\| _ p\\) instead of \\(d _ p(x,y)\\).
</p>
</div>

It is an interesting fact that this distance function, called the \\(p\\)-adic metric, is a valid metric on \\(\{\\mathbf{Z}}\\).

<div class="bd-callout bd-callout-info">
<h4>Exercise</h4>
<p>
Prove that \\(\\|\cdot,\\cdot\\| _ p\\) is a metric.
</p>
</div>

Like every metric, the \\(p\\)-adic metric confers a topology on its underlying set – in this case, \\(\{\\mathbf{Z}}\\). 

### Interlude: Completions

For every metric on a space \\(S\\), there is a notion of "completeness". A space can either be complete, or incomplete, with respect to a given metric.

Consider a sequence \\(a_i \\in S\\) where successive elements come arbitrarily close together, i.e. for any \\(\\epsilon\\), there is some \\(M\\) such that

\\[ i,j>M \\implies |a _ i - a _ j| < \\epsilon \\]

This kind of sequence is called a [*Cauchy* sequence](https://en.wikipedia.org/wiki/Cauchy_sequence).

The question is, do all Cauchy sequences in \\(S\\) *converge*? That is, does there exist some \\(L\\) such that the \\(a _ i\\) come arbitrarily close to \\(L\\)? 

In the language familiar from calculus or analysis, does every Cauchy sequence in \\(S\\) have a limit?  
Well, not necessarily: the standard example is \\(\{\\mathbf{Q}}\\), with the sequence

\\\[1,1.4,1.41,1.414,1.4142,\\ldots\\\]

which does not converge since (modulo some analytical sophistication) \\(\\sqrt 2\\notin\{\\mathbf{Q}}\\).

We say that \\(\{\\mathbf{Q}}\\) is **incomplete** with respect to the standard metric. 

On the other hand, \\(\{\\mathbf{R}}\\) is complete: in fact, the reason why \\(\{\\mathbf{R}}\\) is so important
in mathematics is largely that it is the *completion* of \\(\{\\mathbf{Q}}\\). 

You may be able to guess what that means: there is a procedure, called **(metric) completion**, which starts with a possibly-incomplete metric space
and produces a complete one. This satisfies some "obvious rules": for instance, completing an already-complete space does nothing (up to unique isomorphism, as always).
The construction can be summarized as follows: take the set of all Cauchy sequences in \\(S\\), denoted \\({\\sf Cau}(S)\\). Define a metric on this space (yes!) as follows:

\\[d(a, b) = \\lim _ {n\\to\\infty} d(a _ n, b _ n)\\]

This is, as you can check, a Cauchy sequence of real numbers, and hence the limit is well-defined. We can now define an equivalence relation on \\({\\sf Cau}(S)\\) by saying

\\[a\\sim b \\; \\text{ if } \\; d(a,b) = 0\\]

(which we think of as sequences that are "eventually the same") and define \\(S'\\), the completion of \\(S\\), as the set of equivalence classes. 

The completion of \\(\{\\mathbf{Z}}\\) with respect to this metric is – you guessed it – \\(\{\\mathbf{Z}} _ p\\)! 

## Constructing \\(\{\\mathbf{Q}} _ p\\) from \\(\{\\mathbf{Z}} _ p\\)

Now we can construct the *fraction field* \\(\\Frac {{\\mathbf{Z}}_p}\\).

### Interlude: Fraction fields[^ffwiki]

An integral domain is a ring without *zerodivisors* -- that is, rings where \\(ab = 0\\) implies that at least one of \\(a\\) or \\(b\\) is zero.

Given an integral domain \\(R\\), take the set \\(R'\\) of "formal fractions" \\(\\frac ab\\) where \\(a,b\\in\\mathbf R\\) and \\(b\\neq 0\\). We define two such fractions 
\\(a/b\\) and \\(c/d\\) to be equivalent if 

\\[ ad = bc \\]

(which is basically the idea that \\(2/3 = 4/6 \\in\{\\mathbf{Q}}\\)). The set of equivalence classes is called \\({\\rm {Frac}}(R)\\), the *fraction field* of \\(R\\).

For example, \\(\{\\mathbf{Q}}\\) is the fraction field of \\(\{\\mathbf{Z}}\\).

We have the result

\\[{\\rm Frac}(\{\\mathbf{Z}} _ p) = \\mathbf Q _ p,\\]

the field of \\(p\\)-adic rationals. These are the other completions of \\(\{\\mathbf{Q}}\\) (besides \\(\{\\mathbf{R}}\\), which we've already met).

## \\(\{\\mathbf{Z}} _ p\\) is compact

(Aka "Fascinating fact I can’t prove yet #443" in the original.)

<div class="bd-callout bd-callout-info">
<h4>Miracle</h4>
<p>
It turns out that \\(\{\\mathbf{Z}} _ p\\) can be given a topology. 
</p>
</div>

Since 

\\[\{\\mathbf{Z}} _ p\\subset\\prod\{\\mathbf{Z}}/p ^ k\{\\mathbf{Z}},\\]

you can give the \\(\{\\mathbf{Z}}/p ^ k\{\\mathbf{Z}}\\)s the discrete topology, give the product the product topology, and finally give \\()\{\\mathbf{Z}} _ p\\) the subspace topology. Apparently, this space is compact, by something called [Tychonoff’s theorem](https://en.wikipedia.org/wiki/Tychonoff's_theorem).

## Conclusion

That’s mostly it. I’ll leave it to you to ruminate over these things. I don’t know nearly enough to be able to say anything intelligent[^zp] about \\(\{\\mathbf{Z}} _ p\\) yet, so I’m just writing down whatever I know to ensure that I know that much correctly. Please tell me if I’ve made any mistakes.

This stuff is fascinating – apparently it’s useful in advanced number theory. I can’t wait to learn more!

---

That concludes the original post.

## Acknowledgements

Thanks to the awesome Balarka Sen and @anon over at Math.SE chat for teaching me this stuff. I knew very little ring theory at the time, and generally had much more trouble following mathematical arguments than I do now.

Thanks to [/u/chebushka](rcc) for finding multiple errors in this post, and [/u/samloveshummus](rc1), [/u/aristotle2600](rc2), and [/u/Torpluss](rc3) for catching typos in the old version.

[rc1]: https://www.reddit.com/r/math/comments/3c6f52/i_learned_a_little_about_padics_recently_and/csspc43/
[rc2]: https://www.reddit.com/r/math/comments/3c6f52/i_learned_a_little_about_padics_recently_and/cssqvhm/
[rc3]: https://www.reddit.com/r/math/comments/3c6f52/i_learned_a_little_about_padics_recently_and/cssqw1i/
[rcc]: https://www.reddit.com/r/math/comments/5jw4ws/constructing_the_field_of_padic_numbers/dbji5ia/

[^zp]: This is still true, for a slightly different value of "intelligent".
[^adele]: Adeles were far-off mysteries to me then; things I hadn't even heard of.
[^ffwiki]: The [Wikipedia page](https://en.wikipedia.org/wiki/Field_of_fractions#Construction) treats this well.
[^gen]: The \\(s\\) and \\(t\\) ensure that everything works out even if \\(R\\) contains zerodivisors.
[^lim]: This is [sneakily](https://en.wikipedia.org/wiki/P-adic_number#Algebraic_approach) an "inverse limit", [whatever that means](https://en.wikipedia.org/wiki/Inverse_limit).

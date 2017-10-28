---
title: Polymorphism, variance, and subtyping in Haskell
date: 2017-10-27
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

```hs
if_then_else :: Bool -> Char -> Char -> Char
```

# Subtyping

Subtyping is a kind of polymorphism in the broad sense: it allows one type to be
passed to a function or type constructor in place of another.

A *type constructor* is something like `std::vector` from C++ or `ArrayList` 
from Java which takes a type and constructs a type from it. Using integer types
as "arguments", we get the types `std::vector<int>` and `ArrayList<Integer>`
respectively.

I'll focus on function subtyping here, the reasons for which will become clear
in a moment. Consider a type `Animal`, and a function acting on it, as follows:

```java
class Animal {
  String name;

  public Animal(String name) {
    this.name = name;
  }
}

void printName(Animal a) {
  System.out.println(a.name);
}
```

Now, if we have a type `Dog`,

```java
class Dog extends Animal {
  void bark(String bork) {
    // ...
  }
}
```

it is conceivable that `printName` might let us use it like so:

```java
Dog dog = new Dog("Borkimus 9000");
printName(dog);
```

The reason for this belief, intuitively, is that `Dog` supports everything
`Animal` does: namely, it has the `name` field that `printName` needs.

Informally, this tells us that `Dog` is a subtype of `Animal`.


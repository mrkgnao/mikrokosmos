---
title: Polymorphism, variance, and subtyping in Haskell
date: 2017-10-27
---

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


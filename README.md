Based on the following article from Oleg Kiselyov:
[Effects Without Monads: Non-determinism — Back to the Meta Language](https://arxiv.org/abs/1905.06544).
See also http://okmij.org/ftp/tagless-final/nondet-effect.html

---

The paper does not really present a novel way to deal with effects, but uses this problem as a starting
point to discuss how to apply embedded DSLs to this problem. It claims that the monadic interface is not
only useless but even harmful because supporting it reduces the space of possible effect interpreters.
This is illustrated with two particular interpreters: performing a static analysis of expressions without
evaluating them (abstract interpretation), and generating (possibly more efficient) code from expressions.

---

The paper uses MetaOCaml to implement these ideas. My goal was to see how this code would translate to
idiomatic Scala 3 code, using enums, parameterized traits, extension methods, and staging (for run-time
code generation).

The project is written as a single [literate program](/src/main/scala/effects/Main.scala).

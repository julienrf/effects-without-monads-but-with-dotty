// Based on the following article:
// Effects Without Monads: Non-determinism — Back to the Meta Language
// https://arxiv.org/abs/1905.06544 (see also http://okmij.org/ftp/tagless-final/nondet-effect.html)
// Comments between double quotes (`“` and `”`) are verbatim citations from the article.

// Let’s start with the following program, which computes a permutation of a given list
object Motivation {
  import scala.util.Random

  /**
    * Computes a permutation of `xs` by inserting each element `x` at an arbitrary
    * position of the resulting list.
    */
  def perm(xs: List[Int]): List[Int] = xs.foldLeft(Nil)(insert)

  /**
    * Inserts element `x` at an arbitrary position in `xs`.
    */
  def insert(xs: List[Int], x: Int): List[Int] = xs match {
    case Nil     => x :: Nil
    case y :: ys => (x :: y :: ys) ||| (y :: insert(ys, x))
  }

  /**
    * Non-deterministic operation that evaluates either to its left-hand
    * side, or its right-hand side.
    *
    * Note that this operation is not referentially transparent: subsequent
    * invocations with the same arguments are not guaranteed to return the
    * same result. (That’s on purpose!)
    */
  def (left: => A) ||| [A](right: => A): A =
    if Random.nextBoolean() then left else right

  // As you can see in the console, printing `perm(xs)` might print two different
  // permutations of the given list.
  val xs = 1 :: 2 :: 3 :: Nil
  println(perm(xs)) // List(2, 1, 3)
  println(perm(xs)) // List(2, 3, 1)

}

// It is worth noting that the above program does rely on the effectful `|||` operation.
// Let’s use this starting point to reflect on programming with complicated effects.

// Should we rewrite `perm` to be a ‘pure program’? If so, how should we proceed?

// One popular way to structure effectful programs is to use monads. Let’s try that.
object Monadic {

  def perm(xs: List[Int]): Random[List[Int]] =
    xs.foldLeft(Nil.pure)((rxs, x) => rxs.flatMap(xs => insert(xs, x)))

  def insert(xs: List[Int], x: Int): Random[List[Int]] = xs match {
    case Nil     => (x :: Nil).pure
    case y :: ys => ((x :: y :: ys).pure ||| insert(ys, x).map(y :: _)).flatMap(rxs => rxs)
  }

  def (left: => A) ||| [A](right: => A): Random[A] =
    for cond <- Random.bool yield
      if cond then left else right

  // Example of use
  val xs = 1 :: 2 :: 3 :: Nil
  val program =
    for {
      p1 <- perm(xs)
      p2 <- perm(xs)
    } yield (p1, p2)
  val (_, (p1, p2)) = program.run(1)
  println(p1) // List(2, 3, 1)
  println(p2) // List(3, 1, 2)

  // A monad for working with random values
  final class Random[A](val run: Int => (Int, A)) {
    def flatMap[B](f: A => Random[B]): Random[B] =
      Random(x => { val (y, a) = this.run(x); f(a).run(y) })
    def map[B](f: A => B): Random[B] =
      Random(x => { val (y, a) = this.run(x); (y, f(a)) })
  }
  def (value: A) pure[A]: Random[A] = Random(x => (x, value))
  object Random {
    def int: Random[Int] = Random { x => val y = x * 22695477 + 1; (y, y) }
    def bool: Random[Boolean] = int.map(_ % 2 == 0)
  }

}

// See how we have to insert `map` and `flatMap` calls every here and there.
// “It is hard to imagine preferring the monadic program for its verbose notation.”

// Are the benefits of purity worth it…?
object Purity {
  import Monadic._

  // Let’s define a simple program that takes a `Random[Int]` value as parameter
  def program(randomInt: Random[Int]) =
    for {
      x <- randomInt
      y <- randomInt
    } yield x + y

  // Since `program` has to evaluate its argument `randomInt` anyway, one may be
  // very tempted to ‘optimize’ `program` as follows:
  def optimizedProgram(randomInt: Random[Int]) =
    for {
      x <- randomInt
      y <- x.pure
    } yield x + y

  // “That is, we share the result of the computation rather than the computation
  // `randomInt` itself – and, inadvertently, change the behavior of our program.”

  // “In this simple example, the problem is rather apparent. On one hand, one
  // should not be too surprised: higher-order facility – just like the C
  // preprocessor – gave us the ability to abstract computations rather than values.
  // Functions such as `program`, like C macros, can be rather subtle: their
  // seemingly straightforward refactoring often leads to subtle bugs. To be sure,
  // this is not the problem created by monads – yet monads do little to ameliorate
  // it. When we write effectful code – monads or no monads – we have to constantly
  // keep in mind the context of expressions we pass around”, says Oleg Kiselyov.
}

// Another common pattern is to abstract over the specific `Random` monad implementation
// (that is, to replace every `Random[X]` occurrence in the above program with an abstract
// `F[X]` type, for which there exists a `Monad[F]` instance). Advocates of this pattern
// might be attracted by “the separation of effectful and non-effectful code and the
// possibility of multiple interpretations of the effects”.

// “These good properties are not unique to monads. The other, often forgotten, ways of
// dealing with side-effects ought to be more widely known”, says Oleg Kiselyov.

// “This position paper seeks to draw attention to a non-monadic alternative: rather than
// structuring (effectful) programs as monads -- or applicatives, arrows, etc., -- we
// approach programming as a (micro) language design. We determine what data structures
// and (effectful) operations seem indispensable for the problem at hand -- and design a
// no-frills language with just these domain-specific features.”

/**
  * “We start by designing a language just expressive enough for our problem of computing
  * a list permutation using non-determinism”
  */
trait NonDeterminism {
  // “A language is thus defined by specifying the semantic domain types and the meaning
  // computations for its syntactic form”

  // In Scala, semantic domains can be represented by abstract type members:
  
  /** Integer expressions */
  type IntExpr

  // And, syntax for constructing expressions or combining them together is
  // defined by operations:

  /**
    * Integer literal from a Scala `Int` value.
    *
    * Whereas `1` is a Scala integer literal, `int(1)` represents an integer
    * literal in the `NonDeterminism` language.
    */
  def int(x: Int): IntExpr
  
  /** Integer list expressions */
  type IntListExpr
  /** The empty list of integers */
  def nil: IntListExpr
  /** List constructor */
  def (h: IntExpr) :: (t: IntListExpr): IntListExpr
  /** List literals */
  def list(xs: List[Int]): IntListExpr
  
  /**
    * “Recursively analyze/deconstruct lists.”
    *
    * The meaning of this operation is specified by the following algebraic identities:
    *
    *            nil.recur(f, z) == z
    *       (h :: t).recur(f, z) == f(h, t, () => t.recur(f, z))
    *
    * It is more general than “fold” operations because the caller can control whether to
    * continue the traversal or to stop it.
    */
  def (xs: IntListExpr) recur(f: (IntExpr, IntListExpr, () => IntListExpr) => IntListExpr, z: IntListExpr): IntListExpr
  
  /** Non-deterministic choice between `xs` and `ys` */
  def (xs: IntListExpr) ||| (ys: IntListExpr): IntListExpr
  /** Non-deterministic failure */
  def fail: IntListExpr
  
}

// Examples of list expressions using the `NonDeterminism` language
trait NonDeterminismExamples extends NonDeterminism {

  // Literal lists
  def e1: IntListExpr = int(1) :: int(2) :: int(3) :: nil
  def e2 = list(4 :: 5 :: 6 :: Nil)
  // Non-determinism choice between expressions
  def e3 = e1 ||| e2
  def e4 = e1 ||| e2 ||| fail

  // Consider the following definition:
  def e5 = 1 :: 2 :: 3 :: Nil
  // Note that `e5` is unrelated to `e1`.
  // `e1` is a list expression in the `NonDeterminism` “language”,
  // whereas `e5` is a list in the Scala “meta-language”

}

// We can now check that the `NonDeterminism` language is powerful enough to implement
// list permutation.
// Recall the implementation of `perm`, above:
//
//     def perm(xs: List[Int]): List[Int] = xs.foldLeft(Nil)(insert)
//
// So, we need to be able to express `insert` and `foldLeft` on `IntListExpr`.

trait Perm extends NonDeterminism {

  /**
    * A `foldRight` operation on `IntListExpr`.
    *
    * Note that we don’t need a polymorphic `foldRight`: to implement `perm`, we always return
    * an `IntListExr`.
    *
    * @param xs integer list expression to apply the `foldRight` operation to
    * @param z  initial result value
    * @param op function combining each `IntExpr` element of `xs` with the result of the
    *           previous iteration (starting with `z`).
    */
  def (xs: IntListExpr) foldRight(z: IntListExpr)(op: (IntExpr, IntListExpr) => IntListExpr): IntListExpr =
    xs.recur((h, _, r) => op(h, r()), z)
  
  /**
    * The `insert` operation on `IntListExpr`.
    */
  def insert(x: IntExpr, xs: IntListExpr): IntListExpr =
    xs.recur(((h, t, inserted) => (x :: h :: t) ||| (h :: inserted())), x :: nil)

  // Finally, we can express the `perm` operation in terms of `foldRight`, `nil`, and `insert`
  def perm(xs: IntListExpr) = xs.foldRight(nil)(insert)

}

// Examples of expressions using the `Perm` language
trait PermExamples extends Perm with NonDeterminismExamples{

  def p1 = perm(int(1) :: int(2) :: int(3) :: nil)
  def p2 = perm(list(4 :: 5 :: 6 :: Nil))
  def q  = insert(int(0), list(1 :: 2 :: 3 :: Nil))
  
}

// We have been able to define a program that computes a list permutation in our
// `NonDeterminism` language, however we can not execute that program because
// we have no semantics for the `NonDeterminism` language yet.

// Semantics can be added by fixing the abstract type members `IntExpr` and `IntListExpr`
// and implementing the operations accordingly:
trait NonDeterminismList extends NonDeterminism {
  
  /** 
    * In this interpreter, `IntListExpr` is the list of all the choices that a list
    * expression may produce.
    */
  type IntListExpr = Set[List[Int]]
  /** An integer expression is represented by the integer itself */
  type IntExpr = Int
  def int(x: Int) = x
  
  /** A unique choice containing the empty list */
  def nil = Set(Nil)
  /** `x` prepended to all the choices of `t` */
  def (x: IntExpr) :: (t: IntListExpr) = t.map(x :: _)
  /** A unique choice `xs` */
  def list(xs: List[Int]) = Set(xs)
  
  def (xs: IntListExpr) recur(f: (IntExpr, IntListExpr, () => IntListExpr) => IntListExpr, z: IntListExpr): IntListExpr =
    xs.flatMap {
      case Nil    => z
      case h :: t => f(h, list(t), () => list(t).recur(f, z))
    }
  
  /** No choice at all */
  def fail = Set.empty
  /** Concatenate the choices `xs` with the choices `ys` */
  def (xs: IntListExpr) ||| (ys: IntListExpr) = xs union ys
}

// We can apply the `NonDeterminismList` interpreter to the expressions defined
// in the `Perm` trait as follows:

object PermList extends Perm with NonDeterminismList {

  println(perm(int(1) :: int(2) :: int(3) :: nil)) // Set(List(1, 2, 3), List(1, 3, 2), List(3, 2, 1), List(3, 1, 2), List(2, 3, 1), List(2, 1, 3))
  println(perm(list(4 :: 5 :: 6 :: Nil))) // Set(List(5, 6, 4), List(6, 4, 5), List(5, 4, 6), List(4, 5, 6), List(4, 6, 5), List(6, 5, 4))
  println(insert(int(0), list(4 :: 5 :: Nil)))  // Set(List(0, 4, 5), List(4, 0, 5), List(4, 5, 0))

}

// “Although the `NonDeterminism` DSL is meant for non-deterministic computations, it is not
// as generic and expressive as it could be. For example, the `NonDeterminism` trait does
// not define the general monadic ‘flatMap’ and ‘pure’ operations (they were not needed for
// the task at hand). Implementations of `NonDeterminism`, such as `NonDeterminismList`, may
// support these operations and even use them internally – yet not offer them to the DSL
// programmer. The lack of generality has an upside: the `NonDeterminism` DSL admits
// implementations that do not support ‘flatMap’ and ‘pure’ at all. The following presents
// two non-monadic interpretations of `NonDeterminism`, and explains why they are
// interesting and why they fall outside the conventional monadic framework”

// === Abstract Interpretation ===

// “Instead of actually performing a non-deterministic computation, we estimate the number
// of its non-deterministic choices and the possibility of failure. This is an example
// of the static analysis known as abstract interpretation. Our example is realistic: the
// Kiel Curry compiler, for one, performs a similar determinism analysis in order to produce
// efficient code.”

// “Our abstraction domain here is the expression’s ‘degree of non-determinism’.
// It records the possibility of failure and the upper bound on the number of possible
// values.”

// The number of possible values can be a precise value (e.g., 1, 2, 3, etc.), or “many”.
// This motivates the definition of the following `IntOrInf` data type:
enum IntOrInf {
  case Integer(x: Int)
  case Infinity

  def + (that: IntOrInf): IntOrInf = (this, that) match {
    case (Integer(x), Integer(y)) => Integer(x + y)
    case _                        => Infinity
  }

  def * (that: IntOrInf): IntOrInf = (this, that) match {
    case (Integer(x), Integer(y)) => Integer(x * y)
    case _                        => Infinity
  }

  def <= (that: IntOrInf): Boolean = (this, that) match {
    case (_, Infinity)            => true
    case (Infinity, _)            => false
    case (Integer(x), Integer(y)) => x <= y
  }

  def max(that: IntOrInf): IntOrInf = (this, that) match {
    case (Integer(x), Integer(y)) => Integer(if x < y then y else x)
    case _                        => Infinity
  }
}

trait NonDeterminismDegree extends NonDeterminism {
  import IntOrInf.{Integer, Infinity}

  /**
    * “Possibility of failure and the upper bound on the number of possible values.”
    *
    * “For instance, an expression with the estimated degree:
    *
    *     Degree(canFail = true, choices = Integer(5))
    *
    * may in reality finish without failure, with only two possible values.”
    */
  case class Degree(canFail: Boolean, choices: IntOrInf) {
    /**
      * “If `e1` is a list expression with at most 2 choices and `e2` an expression
      * with at most 3 choices, we would like to estimate that `e1 ||| e2` has at
      * most 5 choices.”
      */
    def + (that: Degree): Degree =
      Degree(this.canFail && that.canFail, this.choices + that.choices)
    /**
      * “If `e1` is a list expression with at most 2 choices and `e2` an expression
      * with at most 3 choices, concatenating the lists `e1` and `e2` should have
      * at most 6 choices of result.”
      */
    def * (that: Degree): Degree =
      Degree(this.canFail || that.canFail, this.choices * that.choices)
    /**
      * Least-upper bound between `this` and `that`.
      */
    def max(that: Degree): Degree =
      Degree(this.canFail || that.canFail, this.choices max that.choices)
  }

  /**
    * The most deterministic (smallest) degree: can’t fail and has exactly one choice.
    * Incidentally, `deterministic` is an identity element for the `*` operation:
    *    forAll((degree: Degree) => degree * deterministic == degree)
    */
  val deterministic    = Degree(canFail = false, choices = Integer(1))
  /**
    * The least deterministic (highest) degree: can fail and has an infinite number of choices.
    * Incidentally, `nonDeterministic` is an absorbing element for the `*` operation:
    *     forAll((degree: Degree) => degree * nonDeterministic == nonDeterministic) 
    */
  val nonDeterministic = Degree(canFail = true, choices = Infinity)

  /**
    * Integer expressions don’t need a runtime representation in this interpreter.
    * We fix their type to `Unit`
    */
  type IntExpr = Unit
  def int(x: Int): IntExpr = ()
  
  /** Integer list expresions are represented by their degree of non-determinism */
  type IntListExpr = Degree
  /** The empty list is deterministic */
  def nil = deterministic
  /** A list literal is detrministic */
  def list(xs: List[Int]): IntListExpr = deterministic
  /** Adding an element to a list expression does not change its degree of non-determinism */
  def (head: IntExpr) :: (tail: IntListExpr): IntListExpr = tail

  /**
    * “Since our abstraction domain keeps track only of the degree of non-determinism for a list
    * expression but not of the length of the list, the best we can do to approximate `xs.recur(f, z)`
    * is to find the upper bound for `recur(f, z)` when applied to lists of every possible length.”
    *
    * “Since we are computing an approximation of the degree of non-determinism, we would be satisfied
    * with an upper bound, not necessarily the least one. Therefore, we can stop the `max` iteration
    * after some number of steps, returning `nonDeterministic` if the convergence has not been achieved
    * by then.”
    */
  def (xs: IntListExpr) recur(f: (IntExpr, IntListExpr, () => IntListExpr) => IntListExpr, z: IntListExpr): IntListExpr = {
    def loop(acc: IntListExpr, resI: IntListExpr, i: Int): IntListExpr = {
      val resINext = f((), deterministic, () => resI)
      val accNext  = acc max resINext
      if acc == accNext then acc
      else if i > 5     then nonDeterministic
      else                   loop(accNext, resINext, i + 1)
    }
    xs * loop(z, z, 0)
  }

  // Non-determinism is introduced by the `fail` and `|||` operators

  // Introduce the possibility of failure
  def fail: IntListExpr = Degree(canFail = true, choices = Integer(1))
  // Add the number of choices of `xs` and `ys`
  def (xs: IntListExpr) ||| (ys: IntListExpr): IntListExpr = xs + ys

  // “Finally, we should stress that we would not have been able to abstractly interpret
  // the DSL code, had the `NonDeterminism` trait required the monadic operation `flatMap`,
  // whose general signature, recall, is:
  //
  //     def (fa: F[A]) flatMap(f: A => F[B]): F[B]
  //
  // for some parameterized type `F`. The second argument to `flatMap`, the continuation,
  // is to receive the value `A` produced by the computation of the first `flatMap`
  // argument. If `F[A]` is realized as `Degree`, it can never produce any concrete `A`
  // value. Therefore, when abstractly interpreting the `flatMap` expression, we cannot
  // ever invoke, and hence analyze, its continuation. That monadic programs cannot be
  // statically analyzed by choosing a suitable abstract monad interpretation was the main
  // motivation for applicative functors and arrows.”
}

object PermDegree extends Perm with NonDeterminismDegree {

  // Literal lists are deterministic
  val e1 = int(1) :: int(2) :: int(3) :: nil
  println(e1) // Degree(false,Integer(1))
  val e2 = list(4 :: 5 :: 6 :: Nil)
  println(e2) // Degree(false,Integer(1))
  // Using `|||` increases the degree of non-determinism
  val e3 = e1 ||| e2
  println(e3) // Degree(false,Integer(2))
  val e4 = int(20) :: (nil ||| (int(10) :: nil) ||| fail)
  println(e4) // Degree(false,Integer(3))
  // The `foldRight` method does not introduce non-determinism
  println(e1.foldRight(nil)(_ :: _)) // Degree(false,Integer(1))
  // The `perm` and `insert` methods introduce non-determinism, which is always overestimated by our abstract interpreter:
  val p1 = perm(e1)
  println(p1) // Degree(true,Infinity)
  val q1 = insert(int(0), nil)
  println(q1) // Degree(true,Infinity)

}

// === Code Generation ===

// “Rather than evaluating a DSL expression, the following interpreter generates code for
// it.”
// The code can be compiled to JVM-bytecode at run-time, or saved into a file.
// “The interpreter is thus a DSL compiler, turning DSL expressions into ordinary Scala
// code”.
// For code generation, we rely on dotty-staging, which adds the type `Expr[A]`, denoting
// so-called code values: (fragments of) the generated code.

import scala.quoted._
import scala.quoted.staging._
given Toolbox = Toolbox.make(getClass.getClassLoader)

object Staging {

  class CodeExpressions(given QuoteContext) {
    // dotty-staging provides several
    // primitives to build such code values. Quoting a Scala expression:
    val n = '{ 5 + 7 }
    // turning it, ''without evaluating'', into a fragment of generated code. The escape, or
    // splice, is a form of antiquotation. It lets us build code templates with holes in them,
    // to be later filled with other fragments, for example:
    def template(x: Expr[Int], y: Expr[Int]): Expr[Int] =
      '{ if $x > 1 then $y else $y * 2 }
    // And then:
    val program: Expr[Int] = template('{ io.StdIn.readInt() }, n)

    println(program.show) // if io.StdIn.readInt() > 1 then 5 + 7 else (5 + 7) * 2)

    // A more interesting example computes (with a recursive function) a program fragment:
    def powerCode(exponent: Int, x: Expr[Double]): Expr[Double] =
      if exponent == 0          then '{ 1 }
      else if exponent == 1     then x
      else if exponent % 2 == 0 then '{ ${powerCode(exponent / 2, x)} * ${powerCode(exponent / 2, x)} }
      else                           '{ $x * ${powerCode(exponent - 1, x)} }

    val cubeCode: Expr[Double => Double] = '{ (x: Double) => ${powerCode(3, '{x})} }
    val fourthCode: Expr[Double => Double] = '{ (x: Double) => ${powerCode(4, '{x})} }

    println(cubeCode.show) // (x: scala.Double) => x * x * x
    println(fourthCode.show) // (x: scala.Double) => x * x * x * x
  }

  // The `run` operation turns an `Expr[A]` into an `A` by evaluating its code
  val cube: Double => Double = run((new CodeExpressions).cubeCode)
  println(cube(3.0)) // 27.0
}
// You can read more about dotty-staging on the official documentation:
// https://dotty.epfl.ch/docs/reference/metaprogramming/staging.html

// “The interpreter interprets DSL expressions as code values: the fragments of code,
// which, when compiled and executed as part of the complete program will compute
// the expression values.”
trait NonDeterminismCode(given QuoteContext) extends NonDeterminism {

  /** Integer expressions are code fragments returning an `Int` value */
  type IntExpr = Expr[Int]
  def int(x: Int): Expr[Int] = Expr(x)

  // “We could have represented list expressions likewise, as the code to compute the
  // list of all choices:
  //
  //     type IntListExpr = Expr[List[List[Int]]]
  //
  // The experience with abstract interpretation (see `NonDeterminismDegree`) has taught
  // us to analyze, to find out what we can say about the program before running it.
  //
  // We therefore incorporate some analysis (typically called ‘binding-time analysis’)
  // into the DSL compiler, which calls for the more elaborate semantic domain for
  // non-deterministic list expressions:

  /**
    * “Distinguishes the case of statically knowing the number of non-deterministic choices
    * - in particular, knowing that a list expression is in fact deterministic. The literal
    * list expression such as `list(1 :: 2 :: 3 :: Nil)` is clearly deterministic.
    * We note that fact (by representing it with the `Known` case) and use later on code
    * generation. The `Unknown` case of `IntListExpr` corresponds to the statically unknown
    * degree of non-determinism. It contains the code computing the choices at run-time.
    * In contrast, in the `Known` variant the choices are known statically, although the
    * content of each choice is generally not and is to be computed at run-time.”
    * “One may always forget the static knowledge and return the opaque
    * `Expr[List[List[Int]]]` value. That is the purpose of the operation `code`.”
    */
  enum IntListExpr {
    /**
      * The choices for this list expression are statically known, each choice is represented
      * as a code fragment that computes it.
      */
    case Known(exprs: List[Expr[List[Int]]])
    /**
      * The choices for this list expression are not knwon, represent the whole list expression
      * as a code fragment that computes all the choices.
      */
    case Unknown(expr: Expr[List[List[Int]]])

    /**
      * Forget our static knowledge about this list expression and return a code fragment
      * that computes all its choices.
      */
    def code: Expr[List[List[Int]]] = this match {
      case Known(exprs)  => exprs.foldLeft(Expr(List.empty[List[Int]]))((xss, xs) => '{ $xs :: $xss })
      case Unknown(expr) => expr
    }
  }
  import IntListExpr._

  // A statically known expression with exactly one choice containing the empty list
  val nil: IntListExpr = Known('{ List.empty } :: Nil)
  // A statically known expression with exactly one choice containing the `xs` list literal
  def list(xs: List[Int]): IntListExpr = Known(Expr(xs) :: Nil)
  // A variant of `list`, where the list expression is already quoted
  def list(expr: Expr[List[Int]]): IntListExpr = Known(expr :: Nil)

  def (head: IntExpr) :: (tail: IntListExpr): IntListExpr = tail match {
    // Add `head` to each code fragment computing a choice
    case Known(exprs)  => Known(exprs.map(xsExpr => '{ $head :: $xsExpr }))
    // Add `head` to the code fragment computing the choices. The implementation
    // is similar to the above line, but now the `map` call is part of the generated
    // code fragment.
    case Unknown(expr) => Unknown('{ $expr.map(xs => $head :: xs) })
  }
  
  def (e: IntListExpr) recur(f: (IntExpr, IntListExpr, () => IntListExpr) => IntListExpr, z: IntListExpr): IntListExpr = {
    def recurCode(expr: Expr[List[Int]]) = '{
      def loop(xs: List[Int]): List[List[Int]] = xs match {
        case Nil    => ${ z.code }
        case h :: t => ${ f('{h}, Known('{t} :: Nil), () => Unknown('{ loop(t) })).code }
      }
      loop($expr)
    }

     e match {
      case Known(Nil)         => Known(Nil)
      case Known(expr :: Nil) => Unknown(recurCode(expr))
      case other              => Unknown('{ ${ other.code }.flatMap(xs => ${ recurCode('{xs}) }) })
    }
  }
  
  def fail: IntListExpr = Known(List.empty)

  def (e1: IntListExpr) ||| (e2: IntListExpr): IntListExpr = (e1, e2) match {
    case (Known(exprs1), Known(exprs2))   => Known(exprs1 ::: exprs2)
    case (Known(exprs), Unknown(expr))    => Unknown(exprs.foldLeft(expr) { (xss, xs) => '{ $xs :: $xss } })
    case (Unknown(expr), Known(exprs))    => Unknown(exprs.foldLeft(expr) { (xss, xs) => '{ $xs :: $xss } })
    case (Unknown(expr1), Unknown(expr2)) => Unknown('{ $expr1 ::: $expr2 })
  }

}

object PermCode {
  class Generator(given QuoteContext) extends Perm with NonDeterminismCode {
    val literalCode = (int(1) :: int(2) :: int(3) :: nil).code
    println(literalCode.show) // List(1 :: 2 :: 3 :: Nil)

    val nonDeterministicExprCode = (nil ||| (int(1) :: nil)).code
    println(nonDeterministicExprCode.show) // List(1 :: Nil, Nil)

    // Code of a function that computes the permutations of a given list
    val permCode: Expr[List[Int] => List[List[Int]]] = '{ (xs: List[Int]) => ${perm(list('{xs})).code} }
    println(permCode.show) // ... that one is too long to fit in a comment
  }

  val generatedPerm: List[Int] => List[List[Int]] =
    run((new Generator).permCode)

  val xs = 1 :: 2 :: 3 :: Nil
  println(generatedPerm(xs)) // List(List(1, 2, 3), List(2, 1, 3), List(2, 3, 1), List(1, 3, 2), List(3, 1, 2), List(3, 2, 1))
}

object Main {

  def main(args: Array[String]): Unit = {

    println("=== Motivation ===")
    Motivation

    println("=== Monadic ===")
    Monadic

    println("=== PermList ===")
    PermList

    println("=== PermDegree ===")
    PermDegree

    println("=== Staging ===")
    Staging

    println("=== PermCode ===")
    PermCode
    
  }
}

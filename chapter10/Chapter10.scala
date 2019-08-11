
import Chapter7.Par.Par

import language.higherKinds
import scala.sys.Prop

object Chapter10 extends App {

  trait Monoid[A] {
    def op(a1: A, a2: A): A

    def zero: A
  }

  object Monoid {

    val stringMonoid = new Monoid[String] {
      def op(a1: String, a2: String) = a1 + a2
      val zero = ""
    }

    def listMonoid[A] = new Monoid[List[A]] {
      def op(a1: List[A], a2: List[A]) = a1 ++ a2
      val zero = Nil
    }

    // 10.1
    val intAddition: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2
      override def zero: Int = 0
    }

    val intMultiplication: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 * a2
      override def zero: Int = 1
    }

    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
      override def zero: Boolean = false
    }

    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
      override def zero: Boolean = true
    }

    // 10.2
    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
      override def zero: Option[A] = None
    }

    // 10.3
    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      override def op(a1: A => A, a2: A => A): A => A = a1.andThen(a2)
      override def zero: A => A = identity
    }

    import Chapter8._;
    import Chapter7._

    // 10.4
    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Chapter8.Prop =
      Chapter8.Prop.forAll(gen.listOfN(gen.unit(3))) {
        v => m.op(m.op(v(0), v(1)), v(2)) == m.op(v(0), m.op(v(1), v(2))) &&
          m.op(v(0), m.zero) == v(0) &&
          m.op(m.zero, v(0)) == v(0)
    }

    def trimMonoid(s: String): Monoid[String] = ???

    def concatenate[A](as: List[A], m: Monoid[A]): A =
      as.foldRight(m.zero)(m.op)

    // 10.5
    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      as.map(f).foldRight(m.zero)(m.op)

    def foldMap1[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      as.foldRight(m.zero)((x, y) => m.op(f(x), y))

    // 10.6
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      foldMap(as, endoMonoid[B])(f.curried)(z)

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      ???

    def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
      ???

    def ordered(ints: IndexedSeq[Int]): Boolean =
      ???

    sealed trait WC

    case class Stub(chars: String) extends WC

    case class Part(lStub: String, words: Int, rStub: String) extends WC

    def par[A](m: Monoid[A]): Monoid[Par[A]] =
      ???

    def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
      ???

    val wcMonoid: Monoid[WC] = ???

    def count(s: String): Int = ???

    def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
      ???

    def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
      ???

    def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
      ???

    def bag[A](as: IndexedSeq[A]): Map[A, Int] =
      ???
  }

  trait Foldable[F[_]] {

    import Monoid._

    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
      ???

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      ???

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      ???

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      ???

    def toList[A](as: F[A]): List[A] =
      ???
  }

  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
      ???

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
      ???

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      ???
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
      ???

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
      ???

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      ???
  }

  object StreamFoldable extends Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
      ???

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
      ???
  }

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object TreeFoldable extends Foldable[Tree] {
    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      ???

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
      ???

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
      ???
  }

  object OptionFoldable extends Foldable[Option] {
    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
      ???

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
      ???

    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
      ???
  }
}

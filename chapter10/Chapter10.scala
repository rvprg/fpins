
import Chapter7.Par.Par

import scala.language.higherKinds

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

    import Chapter7._
    import Chapter8._

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

    // 10.7
    def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      if (as.isEmpty) {
        m.zero
      } else if (as.size == 1) {
        f(as(0))
      } else {
        val halfs = as.splitAt(as.size / 2)
        m.op(foldMapV(halfs._1, m)(f), foldMapV(halfs._2, m)(f))
      }
    }

    // 10.9
    def ordered(ints: IndexedSeq[Int]): Boolean = {
      if (ints.size <= 1) {
        return true
      }
      def increasing(p: (Int, Int)) = p._1 <= p._2
      def chainer: Monoid[Option[(Int, Int)]] = new Monoid[Option[(Int, Int)]] {
        override def op(a1: Option[(Int, Int)], a2: Option[(Int, Int)]): Option[(Int, Int)] = (a1, a2) match {
          case (Some(a1), Some(a2)) => if (increasing(a1) && increasing(a2) && a1._2 == a2._1) Some((a1._1, a2._2)) else None
          case (Some(a1), None) => Some(a1)
          case (None, Some(a2)) => Some(a2)
        }
        override def zero: Option[(Int, Int)] = None
      }

      val v = foldMapV(ints.zip(ints.tail), chainer)(x => Some(x))
      v != None && increasing(v.get) && (v.get == (ints(0), ints.last))
    }

    sealed trait WC

    case class Stub(chars: String) extends WC

    case class Part(lStub: String, words: Int, rStub: String) extends WC

    // 10.8
    def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
      override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)((x, y) => m.op(x, y))
      override def zero: Par[A] = Par.unit(m.zero)
    }

    def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
      foldMapV(v, par(m))(Par.asyncF(f))

    // 10.10
    val wcMonoid: Monoid[WC] = new Monoid[WC] {
      override def op(a1: WC, a2: WC): WC = (a1, a2) match {
        case (Stub(l), Stub(r)) => Stub(l+r)
        case (Stub(p), Part(l, w, r)) => Part(p + l, w, r)
        case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
        case (Part(l, w1, ""), Part("", w2, r)) => Part(l, w1 + w2, r)
        case (Part(l, w1, _), Part(_, w2, r)) => Part(l, w1 + w2 + 1, r)
      }
      override def zero: WC = Stub("")
    }

    // 10.11
    def count(s: String): Int = {
      def calc(s: String): WC = {
        if (s == " ") {
          Part("", 0, "")
        } else if (s.length > 1) {
          val (l, r) = s.splitAt(s.size/2)
          wcMonoid.op(calc(l), calc(r))
        } else {
          Stub(s)
        }
      }
      def asWordCount(s: String) = if (s.isEmpty) 0 else 1
      calc(s) match {
        case Stub(l) => asWordCount(s)
        case Part(l, w, r) => w + asWordCount(l) + asWordCount(r)
      }
    }

    // 10.16
    def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
      // (a b) op ((c d) op (e f)) = (a b) op (ce, df) = (ace, bdf)
      // ((a b) op (c d)) op (e f) = (ac bd) op (e, f) = (ace, bdf)
      override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
      // (a b) op (azero, bzero) = (aazero bbzero) = (a b)
      override def zero: (A, B) = (A.zero, B.zero)
    }

    // 10.17
    def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B): A => B = x => B.op(a1(x), a2(x))
      override def zero: A => B = x => B.zero
    }


    def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
      def zero = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

    // 10.18
    def bag[A](as: IndexedSeq[A]): Map[A, Int] =
      foldMapV(as, mapMergeMonoid[A, Int](intAddition))((x: A) => Map(x -> 1))

  }

  trait Foldable[F[_]] {

    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
      ???

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      ???

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      ???

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      ???

    // 10.15
    def toList[A](as: F[A]): List[A] =
      foldRight(as)(List[A]())((v, l) => v :: l)
  }

  // 10.12
  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  object StreamFoldable extends Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
      as.foldLeft(z)(f)
  }

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // 10.13
  object TreeFoldable extends Foldable[Tree] {
    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Leaf(v) => f(v)
      case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
      case Leaf(v) => f(z, v)
      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
    }

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =as match {
      case Leaf(v) => f(v, z)
      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
    }
  }

  // 10.14
  object OptionFoldable extends Foldable[Option] {
    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Some(v) => f(v)
      case None => mb.zero
    }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
      case Some(v) => f(z, v)
      case None => z
    }

    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
      case Some(v) => f(v, z)
      case None => z
    }
  }
}

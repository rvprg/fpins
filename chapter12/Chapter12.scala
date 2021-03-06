
import Chapter10.{Foldable, Monoid}
import Chapter11.{Functor}
import Chapter6.State

import language.higherKinds
import language.implicitConversions

object Chapter12 extends App {

  trait Applicative[F[_]] extends Functor[F] {
    // 12.1
    def sequence[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(identity)

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((v, l) => map2(f(v), l)(_ :: _))

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      sequence(List.fill(n)(fa))

    // 12.2
    // In terms of apply + unit
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)

    // In terms of map2
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
      map2(fab, fa)((g, x) => g(x))

    def unit[A](a: => A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)

    // 12.3
    def map3[A, B, C, D](fa: F[A], fb: F[B],
                         fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(apply(unit(f.curried))(fa))(fb))(fc)


    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C],
                            fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)


    def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???

    // 12.8
    def product[G[_]](G: Applicative[G]) = {
      val self = this
      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

        override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
          (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
      }
    }

    // 12.9
    def compose[G[_]](G: Applicative[G]) = {
      val self = this
      new Applicative[({type f[x] = F[G[x]]})#f] {
        override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

        override def apply[A, B](fab: F[G[A => B]])(fa: F[G[A]]): F[G[B]] =
          self.map2(fab, fa)((f, a) => G.map2(f, a)((ff, aa) => ff(aa)))
      }
    }

    // 12.12
    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
      ofa.foldRight(unit(Map[K, V]()))((e, m) => map2(e._2, m)((v, m) => m + (e._1 -> v)))
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(g)

    override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
      flatMap(mf)(f => map(ma)(a => f(a)))
  }

  object Monad {
    // 12.5
    def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }
    }

    def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }

    def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???
  }

  object Applicative {

    val streamApplicative = new Applicative[Stream] {

      def unit[A](a: => A): Stream[A] =
        Stream.continually(a) // The infinite, constant stream

      override def map2[A, B, C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                                                              f: (A, B) => C): Stream[C] =
        a zip b map f.tupled
    }

    // 12.4
    // The sequence for streamApplicative returns a stream, that has the same number of
    // elements as the input list, where each element is the original stream converted
    // to a list.

    sealed trait Validation[+E, +A]

    case class Failure[E](head: E, tail: Vector[E])
      extends Validation[E, Nothing]

    case class Success[A](a: A) extends Validation[Nothing, A]

    // 12.6
    def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
        case (Success(a), Success(b)) => unit(f(a, b))
        case (Failure(h, t), Success(_)) => Failure(h, t)
        case (Success(_), Failure(h, t)) => Failure(h, t)
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, Vector(h2).appendedAll(t1).appended(t2))
      }
    }

    type Const[A, B] = A

    implicit def monoidApplicative[M](M: Monoid[M]) =
      new Applicative[({type f[x] = Const[M, x]})#f] {
        def unit[A](a: => A): M = M.zero

        override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
      }
  }

  trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
    def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
      sequence(map(fa)(f))

    def sequence[G[_] : Applicative, A](fma: F[G[A]]): G[F[A]] =
      traverse(fma)(ma => ma)

    def map[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f)

    import Applicative._
    import StateUtil._

    override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      traverse[({type f[x] = Const[B, x]})#f, A, Nothing](
        as)(f)(monoidApplicative(mb))

    def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
      traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

    def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
      traverseS(fa)((a: A) => (for {
        s1 <- get[S]
        (b, s2) = f(a, s1)
        _ <- set(s2)
      } yield b)).run(s)

    override def toList[A](fa: F[A]): List[A] =
      mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

    def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
      mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

    def reverse[A](fa: F[A]): F[A] = ???

    override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

    def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                              (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

    def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
  }

  object Traverse {
    // 12.13
    val listTraverse = new Traverse[List] {
      def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
        fa.foldRight(G.unit(List[B]()))((v, l) => G.map2(f(v), l)(_ :: _))
    }

    val optionTraverse = new Traverse[Option] {
      def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = fa match {
        case Some(v) => G.map(f(v))(x => Some(x))
        case None => G.unit(None)
      }
    }

    val treeTraverse = new Traverse[Tree] {
      def traverse[G[_] : Applicative, A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
        G.map2(f(fa.head), listTraverse.traverse(fa.tail)(x => traverse(x)(f)))((h, t) => Tree(h, t))
    }
  }

  // 12.14
  trait Traverse_1[F[_]] extends Functor[F] {
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
      sequence(map(fa)(f))

    def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
      traverse(fga)(ga => ga)

    type Id[A] = A
    val idMonad = new Applicative[Id] {
      override def unit[A](a: => A) = a
      def flatMap[A, B](ma: Id[A])(f: A => Id[B]) = f(ma)
    }

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      traverse[Id, A, B](fa)(f)(idMonad, idMonad)
  }

  // The `get` and `set` functions on `State` are used above,
  // but aren't in the `exercises` subproject, so we include
  // them here
  object StateUtil {

    def get[S]: State[S, S] =
      State(s => (s, s))

    def set[S](s: S): State[S, Unit] =
      State(_ => ((), s))
  }

}
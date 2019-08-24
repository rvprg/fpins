
import Chapter7.Par
import Chapter7.Par.Par
import Chapter8.Gen

import language.higherKinds

object Chapter11 extends App {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))

    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
  }

  object Functor {
    val listFunctor = new Functor[List] {
      def map[A, B](as: List[A])(f: A => B): List[B] = as map f
    }
  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => unit(f(a)))

    def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    // 11.3
    def sequence[A](lma: List[M[A]]): M[List[A]] =
      lma.foldRight(unit(List[A]()))((l, x) => map2(l, x)((a, b) => a :: b))

    def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
      la.foldRight(unit(List[B]()))((l, x) => map2(f(l), x)((a, b) => a :: b))

    // 11.4
    def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
      sequence(List.fill(n)(ma))

    // 11.5
    // For List it would return a list of lists
    // For Option is would return a list of either Somes or Nones.

    // 11.6
    def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = ms match {
      case h :: t => flatMap(f(h))(x => if (x) map(filterM(t)(f))(h :: _) else filterM(t)(f))
      case Nil => unit(Nil)
    }

    // 11.7
    def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
      x => flatMap(f(x))(b => g(b))

    // 11.8
    def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
      compose[M[A], A, B](identity, f)(ma)

    // 11.9

    // 11.10

    // 11.11
    // flatMap(Some(v))(unit) = unit(v) = Some(v)
    // flatMap(unit(Some(v))(f) = flatMap(Some(Some(v))(f) = f(Some(v))

    // 11.12
    def join[A](mma: M[M[A]]): M[A] =
      flatMap(mma)(identity)

    // 11.13
    def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
      join(map(ma)(f))
  }

  case class Reader[R, A](run: R => A)

  object Monad {
    val genMonad = new Monad[Gen] {
      def unit[A](a: => A): Gen[A] = Gen.unit(a)

      override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
        ma flatMap f
    }

    // 11.1
    val parMonad: Monad[Par] = new Monad[Par] {
      override def unit[A](a: => A): Par[A] = Par.unit(a)

      override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
    }

    /*
  def parserMonad[P[+_]](p: Parser[P]): Monad[P] = new Monad[P] {
    override def unit[A](a: => A): P[A] = ???
    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = ???
  }*/

    val optionMonad: Monad[Option] = new Monad[Option] {
      override def unit[A](a: => A): Option[A] = Option(a)

      override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
    }

    val streamMonad: Monad[Stream] = new Monad[Stream] {
      override def unit[A](a: => A): Stream[A] = Stream.continually(a)

      override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
    }

    val listMonad: Monad[List] = new Monad[List] {
      override def unit[A](a: => A): List[A] = List(a)

      override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
    }


    def stateMonad[S] = ???

    val idMonad: Monad[Id] = ???

    def readerMonad[R] = ???
  }

  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = ???

    def flatMap[B](f: A => Id[B]): Id[B] = ???
  }

  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
      def unit[A](a: => A): Reader[R, A] = ???

      override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = ???
    }
  }

}
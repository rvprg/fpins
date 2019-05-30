import scala.{Either => _, Left => _, Option => _, Right => _}

object Chapter4 extends App {

  sealed trait Option[+A] { // 4.1
    def map[B](f: A => B): Option[B] = this match {
      case Some(v) => Some(f(v))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(v) => v
      case None => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case Some(_) => this
      case None => ob
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(v) if (f(v)) => this
      case _ => None
    }
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  object Option {
    def variance(seq: Seq[Double]): Option[Double] = { //4.2
      def mean(sum: Seq[Double]): Option[Double] = if (sum.isEmpty) None else Some(sum.sum / sum.length)

      mean(seq).flatMap(m => mean(seq.map(x => math.pow(x - m, 2))))
    }

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match { // 4.3
      case (Some(x), Some(y)) => Some(f(x, y))
      case (_, _) => None
    }

    def map2_c[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = { // 4.3
      a.flatMap(x => b.map(y => f(x, y)))
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] = { // 4.4
      a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)((x, y) => x :: y))
    }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = { // 4.5
      a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)((x, y) => x :: y))
    }

    def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)
  }


  sealed trait Either[+E, +A] { // 4.6
    def map[B](f: A => B): Either[E, B] =
      this match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)
      }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(v) => f(v)
      case Left(e) => Left(e)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(v) => Right(v)
      case Left(_) => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
      case (Right(x), Right(y)) => Right(f(x, y))
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
    }
  }

  case class Left[+E](get: E) extends Either[E, Nothing]

  case class Right[+A](get: A) extends Either[Nothing, A]

  object Either {
    // 4.7
    def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      es.foldRight[Either[E, List[B]]](Right(Nil))((x, y) => f(x).map2(y)((x, y) => x :: y))

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)
  }


}

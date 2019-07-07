import Chapter6._

object Chapter8 extends App {

  /*
  trait Prop {
    def check: Boolean

    // 8.3
    def &&(p: Prop): Prop = new Prop {
      def check = this.check && p.check
    }
  }
  */

  case class Gen[A](sample: State[RNG, A]) {
    // 8.4
    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen[Int](State(RNG.nonNegativeLessThan(stopExclusive - start)).map(v => v + start))
    }

    // 8.5
    def unit[A](a: => A): Gen[A] = Gen(State(s => (a, s)))

    def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeLessThan(2)).map(_ == 1))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

    // 8.6
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(x => f(x).sample))

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(n => listOfN(n, this))

    // 8.7
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(if (_) g1 else g2)

    // 8.8
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val l = g1._2 + g2._2
      val p = g1._2 / l
      Gen(State(RNG.double).flatMap(x => if (x <= p) g1._1.sample else g2._1.sample))
    }

    // 8.9
  }

  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case class Prop(run: (TestCases, RNG) => Result) {
    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
      Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  }



  println("hello")
}

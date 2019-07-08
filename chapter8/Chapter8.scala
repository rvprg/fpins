import Chapter6._
import java.lang.System._

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

  case class SGen[+A](forSize: Int => Gen[A]) {

  }

  case class Gen[+A](sample: State[RNG, A]) {
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

    // 8.10
    def unsized: SGen[A] = SGen {
      s => this
    }

    // 8.11
    // 8.12
    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
      s => listOfN(s, g)
    }

    // 8.13
    def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen {
      s => g.listOfN(if (s > 0) s else 1)
    }
  }

  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (_, n, rng) =>
        randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
          case (a, i) => try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
        }.find(_.isFalsified).getOrElse(Passed)
    }

    //    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    //      forAll(g(_))(f)

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max, n, rng) =>
        val casesPerSize = (n + (max - 1)) / max
        val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop: Prop = props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
        prop.run(max, n, rng)
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
      Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    // 8.9
    def &&(p: Prop): Prop = Prop {
      (max, n, rng) => {
        val resLeft = this.run(max, n, rng)
        if (resLeft.isFalsified) resLeft else p.run(max, n, rng)
      }
    }

    def ||(p: Prop): Prop = Prop {
      (max, n, rng) => {
        val resLeft = this.run(max, n, rng)
        if (resLeft.isFalsified) p.run(max, n, rng) else resLeft
      }
    }
  }

  object Prop {
    def run(p: Prop, maxSize: Int = 100, testCases: TestCases = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed testes:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
      }
  }

  println("hello")
}

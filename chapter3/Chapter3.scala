object Chapter3 extends App {

  def init[A](list: List[A]): List[A] = list match { // 3.6
    case Nil => list
    case _ :: Nil => Nil
    case x :: xs => x :: init(xs)
  }

  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = list match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = list match { // 3.10
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }

  def length[A](list: List[A]) = foldLeft(list, 0: Int)((c, _) => c + 1) // 3.9

  def reverse[A](list: List[A]) = foldLeft(list, Nil: List[A])((l, e) => e :: l) // 3.12

  def append[A](list1: List[A], list2: List[A]): List[A] = foldRight(list1, list2)((e, l) => e :: l) // 3.14

  def concat[A](list: List[List[A]]): List[A] = foldLeft(list, Nil: List[A])(append) // 3.15

  def foldRight2[A, B](list: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(list), z)((a, b) => f(b, a)) // 3.13

  def add1(list: List[Int]): List[Int] = list match { // 3.16
    case Nil => list
    case x :: xs => (x + 1) :: add1(xs)
  }

  def map[A, B](list: List[A])(f: A => B): List[B] = list match { // 3.18
    case Nil => List[B]()
    case x :: xs => f(x) :: map(xs)(f)
  }

  def filter[A](list: List[A])(f: A => Boolean): List[A] = list match { // 3.19
    case Nil => list
    case x :: xs => if (f(x)) x :: filter(xs)(f) else filter(xs)(f)
  }

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list match { // 3.20
    case Nil => Nil
    case x :: xs => append(f(x), flatMap(xs)(f))
  }

  def filter2[A](list: List[A])(f: A => Boolean): List[A] = flatMap(list)(x => if (f(x)) List(x) else Nil) // 3.21

  def addLists(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match { // 3.22
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (x :: xs, y :: ys) => (x + y) :: addLists(xs, ys)
  }

  def zipWith[A, B](list1: List[A], list2: List[A])(f: (A, A) => B): List[B] = (list1, list2) match { // 3.23
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
  }

  def hasSubsequence[A](list: List[A], sub: List[A]): Boolean = (list, sub) match { // 3.24
    case (Nil, _) => false
    case (_, Nil) => true
    case (x :: xs, y :: ys) => {
      if (x == y && hasSubsequence(xs, ys)) true
      else hasSubsequence(xs, sub);
    }
  }

  sealed trait Tree[+A]

  case class Leaf[A](v: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = tree match { // 3.25
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maximum(tree: Tree[Int]): Int = tree match { // 3.26
    case Leaf(v) => v
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match { // 3.27
    case Leaf(_) => 0
    case Branch(left, right) => depth(left) max depth(right) + 1
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match { // 3.28
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(g: A => B)(f: (B, B) => B): B = tree match { // 3.29
    case Leaf(v) => g(v)
    case Branch(left, right) => f(fold(left)(g)(f), fold(right)(g)(f))
  }

  def size1[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((x, y) => x + y + 1)
  def maximum1(tree: Tree[Int]): Int = fold(tree)(_ => 0)((x, y) => x max y)
  def depth1[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((x, y) => x max y + 1)
  def map1[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(x => Leaf(f(x)): Tree[B])((x, y) => Branch(x, y))
}

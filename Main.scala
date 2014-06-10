import org.scalameter.api._
import scalaz._, Scalaz._

trait TreeLike[F[_]] {
  // XXX can ditch call by name?
  def node[A](l: ⇒ F[A], r: ⇒ F[A]): F[A]
}

object Strict {
  sealed trait Tree[A]
  case class Leaf[A](a: A) extends Tree[A]
  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  object Tree {
    implicit object treeMonad extends Monad[Tree] {
      def point[A](a: ⇒ A): Tree[A] = Leaf(a)
      def bind[A, B](fa: Tree[A])(f: A ⇒ Tree[B]) = fa match {
        case Leaf(a) ⇒ f(a)
        case Node(l, r) ⇒ Node(bind(l)(f), bind(r)(f))
      }
    }
  }

  def fullTree(i: Int): Tree[Int] = i match {
    case 1 ⇒ Leaf(1)
    case n ⇒
      val j = n - 1
      for {
        t ← fullTree(j)
        node ← Node(Leaf(j - 1), Leaf(j + 1)): Tree[Int]
      } yield node
  }

  def zigzag(tree: Tree[Int]): Int = {
    def zig(t: Tree[Int]): Int = t match {
      case Leaf(n) ⇒ n
      case n: Node[Int] ⇒ zag(n.l)
    }
    def zag(t: Tree[Int]): Int = t match {
      case Leaf(n) ⇒ n
      case n: Node[Int] ⇒ zig(n.r)
    }
    zig(tree)
  }
}

object Lazy {
  sealed trait Tree[A]
  case class Leaf[A](a: A) extends Tree[A]
  class Node[A](_l: ⇒ Tree[A], _r: ⇒ Tree[A]) extends Tree[A] {
    lazy val l = _l
    lazy val r = _r
  }

  object Tree {
    implicit object treeMonad extends Monad[Tree] {
      def point[A](a: ⇒ A): Tree[A] = Leaf(a)
      def bind[A, B](fa: Tree[A])(f: A ⇒ Tree[B]) = fa match {
        case Leaf(a) ⇒ f(a)
        case n: Node[A] ⇒ new Node(bind(n.l)(f), bind(n.r)(f))
      }
    }

    implicit object treeTreeLike extends TreeLike[Tree] {
      def node[A](l: ⇒ Tree[A], r: ⇒ Tree[A]): Tree[A] =
        new Node(l, r)
    }
  }

  def fullTree[F[_]: Monad](i: Int)(implicit F: TreeLike[F]): F[Int] =
    i match {
      case 1 ⇒ 1.point[F]
      case n ⇒
        val j = n - 1
        for {
          t ← fullTree[F](j)
          //l = (j - 1).point[F]
          //r = (j + 1).point[F]
          //node ← F.node(l, r)
          node ← F.node((j - 1).point[F], (j + 1).point[F])
        } yield node
    }

  def zigzag(tree: Tree[Int]): Int = {
    def zig(t: Tree[Int]): Int = t match {
      case Leaf(n) ⇒ n
      case n: Node[Int] ⇒ zag(n.l)
    }
    def zag(t: Tree[Int]): Int = t match {
      case Leaf(n) ⇒ n
      case n: Node[Int] ⇒ zig(n.r)
    }
    zig(tree)
  }
}

object CodensityStuff {
  import Lazy._
  type CodensityTree[A] = Codensity[Tree, A]
  implicit object codensityTreeTreeLike extends TreeLike[CodensityTree] {
    def node[A](l: ⇒ CodensityTree[A], r: ⇒ CodensityTree[A]): CodensityTree[A] =
      new Codensity[Tree, A] {
        def apply[B](f: A ⇒ Tree[B]) =
          implicitly[TreeLike[Tree]].node(l.apply(f), r.apply(f))
      }
  }
}

object TreeBenchmark extends PerformanceTest {
  lazy val executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val reporter =
    ChartReporter(ChartFactory.XYLine())
    //new LoggingReporter
  lazy val persistor = Persistor.None

  performance of "fullTree" in {
    measure method "strict" in {
      val strictTreeSizes = Gen.range("strict size")(2, 20, 1)
      using(strictTreeSizes) in { n ⇒
        import Strict._
        val tree = fullTree(n)
        zigzag(tree)
      }

      val lazyTreeSizes = Gen.range("lazy size")(2, 2000, 10)
      using(lazyTreeSizes) in { n ⇒
        import Lazy._
        val tree = fullTree[Tree](n)
        zigzag(tree)
      }

      using(lazyTreeSizes) in { n ⇒
        import CodensityStuff._
        import Lazy._
        val tree = fullTree[CodensityTree](n)
        zigzag(tree.improve)
      }
    }
  }
}

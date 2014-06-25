import language._
import org.scalameter.api._
import scalaz._, Scalaz._

trait FreeLike[M[_[_], _]] {
  implicit def monad[F[_]: Functor]: Monad[({ type l[a] = M[F, a] })#l]
  def liftFree[F[_]: Functor, A](fa: F[A]): M[F, A]
  def wrap[F[_]: Functor, A](fa: F[M[F, A]]): M[F, A] =
    liftFree(fa).join
  implicit class syntax[F[_]: Functor, A](fa: F[A]) {
    def liftFree: M[F, A] = FreeLike.this.liftFree(fa)
  }
}

object FreeLike {
  def apply[M[_[_], _]: FreeLike]: FreeLike[M] = implicitly[FreeLike[M]]

  implicit object freeFreeLike extends FreeLike[Free] {
    def monad[F[_]: Functor] = Monad[({ type l[a] = Free[F, a] })#l]
    def liftFree[F[_]: Functor, A](fa: F[A]) =
      Free.Suspend(Functor[F].map(fa)(a => Free.Return(a)))
  }

  implicit object sfreeFreeLike extends FreeLike[SFree] {
    def monad[F[_]: Functor] = SFree.sfreeMonad[F]
    def liftFree[F[_]: Functor, A](fa: F[A]) =
      SWrap(Functor[F].map(fa)(a => SPure(a)))
  }

  type CFL[M[_[_], _], F[_], A] = Codensity[({ type l[a] = M[F, a] })#l, A]
  class CflFreeLike[M[_[_], _]](implicit FLM: FreeLike[M]) extends FreeLike[({ type l[f[_], a] = Codensity[({ type m[b] = M[f, b] })#m, a] })#l] {
    def monad[F[_]: Functor]: Monad[({ type l[a] = Codensity[({ type m[b] = M[F, b] })#m, a] })#l] =
      Codensity.codensityMonad[({ type mf[a] = M[F, a] })#mf]
    def liftFree[F[_], A](fa: F[A])(implicit F: Functor[F]): Codensity[({ type mf[a] = M[F, a] })#mf, A] = {
      type MF[A] = M[F, A]
      import FLM.monad
      new Codensity[MF, A] {
        def apply[B](f: A => MF[B]): MF[B] =
          FLM.monad[F].bind(FLM.liftFree(fa))(f)
      }
    }
  }
  implicit def cflFreeLike[M[_[_], _]: FreeLike]: FreeLike[({ type l[f[_], a] = Codensity[({ type m[b] = M[f, b] })#m, a] })#l] =
    new CflFreeLike[M]
}

sealed trait SFree[F[_], A] {
  def map[B](f: A => B)(implicit F: Functor[F]): SFree[F, B]
  def flatMap[B](f: A => SFree[F, B])(implicit F: Functor[F]): SFree[F, B]
  def foldMap[M[_]:Monad](f: F ~> M): M[A]
}
case class SPure[F[_], A](a: A) extends SFree[F, A] {
  def map[B](f: A => B)(implicit F: Functor[F]) = SPure[F, B](f(a))
  def flatMap[B](f: A => SFree[F,B])(implicit F: Functor[F]) = f(a)
  def foldMap[M[_]:Monad](f: F ~> M) = Monad[M].point(a)
}
case class SWrap[F[_], A](a: F[SFree[F, A]]) extends SFree[F, A] {
  def map[B](f: A => B)(implicit F: Functor[F]) = SWrap(F.map(a)(_.map(f)))
  def flatMap[B](f: A => SFree[F,B])(implicit F: Functor[F]) = SWrap(F.map(a)(_.flatMap(f)))
  def foldMap[M[_]:Monad](f: F ~> M) =
    Monad[M].bind(f(a))(_.foldMap[M](f))
    
}
object SFree {
  implicit def sfreeMonad[F[_]: Functor] = new Monad[({ type l[a] = SFree[F, a] })#l] {
    def point[A](a: => A) = SPure[F, A](a)
    override def map[A, B](fa: SFree[F, A])(f: A => B) = fa.map(f)
    def bind[A,B](fa: SFree[F,A])(f: A => SFree[F,B]) = fa.flatMap(f)
  }
}

object Stuffs {
  type Username = String
  case class User(name: Username)
  def users(n: Int): List[Username] =
    List.range(0, n).map(i => s"user-$i")
  def user[M[_]: Monad](username: Username): M[User] =
    User(username).point[M]

  def gof[F[_[_],_], M[_]](n: Int)(implicit F: FreeLike[F], M: Monad[M]): F[M, List[User]] = {
    import F.{ monad, syntax }
    users(n).traverseU(username => user[M](username).liftFree)
  }
}

object FreeBenchmark extends PerformanceTest {
  lazy val executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val reporter = ChartReporter(ChartFactory.XYLine())
  lazy val persistor = Persistor.None

  performance of "stuff" in {
    measure method "misc" in {
      val sizes = Gen.range("sizes")(0, 1000, 25)
      val flags = (
        exec.jvmflags -> "-Xmx4g -Xms4g -XX:CompileThreshold=100 -Xss64m"
      )
      import NaturalTransformation._

      using(sizes) config flags in { n =>
        Stuffs.gof[SFree, Id](n).foldMap(id).copoint
      }
      
      using(sizes) config flags in { n =>
        type CFLSFree[F[_], A] = FreeLike.CFL[SFree, F, A]
        Stuffs.gof[CFLSFree, Id](n).improve.foldMap(id).copoint
      }

      using(sizes) config flags in { n =>
        Stuffs.gof[Free, Id](n).foldMap(id).copoint
      }
    }
  }
}

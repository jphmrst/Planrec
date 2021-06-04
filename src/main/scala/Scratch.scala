import scala.collection.mutable.{Builder, HashMap, HashSet}

trait MMM[X, Y]
trait PPP[X, Y]
trait QQQ[X, Y, Z]
object Scratch extends App {

  trait AA[Setter[_], Mapper[_,_], Elements[_,_], Automaton[_,_]] {
    def build[S,T](): Builder[Elements[S,T], Automaton[S,T]]
  }

  given AA[HashSet, HashMap, MMM, [X,Y] =>> QQQ[X, Y, PPP[Set[X], Y]]] with {
    override def build[S,T](): Builder[MMM[S, T], QQQ[S, T, PPP[Set[S], T]]] =
      ???
  }

}

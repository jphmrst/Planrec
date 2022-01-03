// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr
import scala.collection.mutable.Queue
import org.maraist.planrec.rules.HTNLib
import org.maraist.planrec.rules.HTN.HTNrule
import org.maraist.planrec.terms.Term.*
import org.maraist.planrec.rules.HTN.*

class Table[T, H, S](
  initialIndex: Int,
  val termHeads: Vector[H],
  val states: Vector[Set[HState[T, H, S]]],
  private val headColMap: Map[H, Int],
  private val stateRowMap: Map[Set[HState[T, H, S]], Int],
  private val shifts: Array[Array[Option[Int]]],
  private val reduces: Array[Array[List[(Int, Int)]]],
  private val sparks: Array[Array[List[List[Int]]]]
) /* (using TermImpl[T, H, S]) */ {
  opaque type Row = Int
  opaque type Col = Int

  val headCol: Map[H, Col] = headColMap

  val stateRow: Map[Set[HState[T, H, S]], Row] = stateRowMap

  inline def rowShift(state: Row, termHead: Col): Option[Row] =
    shifts(state)(termHead)

  inline def rowReduces(state: Row, termHead: Col): List[(Row, Int)] =
    reduces(state)(termHead)

  inline def rowSparks(state: Row, termHead: Col): List[List[Row]] =
    sparks(state)(termHead)

  val initial: Row = initialIndex
}

object Table {
  def apply[T, H, S](library: HTNLib[T, H, S])(using TermImpl[T, H, S]):
      Table[T, H, S] = {

    val dfa = HandleDFA(library)
    val labels: IndexedSeq[H] = dfa.labels
    val states: IndexedSeq[Set[HState[T, H, S]]] = dfa.states

    val headCol: Map[H, Int] = {
      val builder = Map.newBuilder[H, Int]
      for (i <- 0 until labels.length)
        do builder += ((labels(i), i))
      builder.result
    }

    val stateRow: Map[Set[HState[T, H, S]], Int] = {
      val builder = Map.newBuilder[Set[HState[T, H, S]], Int]
      for (i <- 0 until states.length)
        do builder += ((states(i), i))
      builder.result
    }

    val shifts = Array.tabulate(states.length, labels.length)((s, l) =>
      dfa.transitionIndex(s, l))

    val reduces = Array.tabulate(states.length, labels.length)((s, l) => {
      val buf = List.newBuilder[(Int, Int)]

      def pullFinalItem(item: Item[T, H, S])(using TermImpl[T, H, S]): Unit = {
        if item.isFinal then
          buf += ((headCol(item.rule.goal.termHead), item.rule.length))
      }

      dfa.transitionIndex(s, l) match {
        case None => { }
        case Some(r) => for(elem <- dfa.state(r)) do elem match {
          case Station(_) => { }
          case Sparking(_, item): Sparking[T, H, S] => pullFinalItem(item)
          case i@AllItem(_, _, _): AllItem[T, H, S] => pullFinalItem(i)
          case i@OneItem(_, _): OneItem[T, H, S] => pullFinalItem(i)
          case i@ActItem(_, _): ActItem[T, H, S] => pullFinalItem(i)
        }
      }

      buf.result
    })

    val sparks = Array.tabulate(states.length, labels.length)((s, l) => {
      val buf = List.newBuilder[List[Int]]

      dfa.transitionIndex(s, l) match {
        case None => { }
        case Some(r) => for(elem <- dfa.state(r)) do elem match {
          case Sparking(indirs, _): Sparking[T, H, S] => ???
          case _ => { }
        }
      }

      buf.result
    })

    new Table(
      dfa.initialStateIndex, labels.toVector, states.toVector,
      headCol, stateRow, shifts, reduces, ???)
  }
}

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
  val termHeads: Vector[H],
  val states: Vector[Set[HState[T, H, S]]],
  private val headColMap: Map[H, Int],
  private val stateRowMap: Map[Set[HState[T, H, S]], Int],
  private val shifts: Array[Array[Int]],
  private val reduces: Array[Array[List[Int]]],
  private val sparks: Array[Array[List[Int]]]
) /* (using TermImpl[T, H, S]) */ {
  opaque type Row = Int
  opaque type Col = Int

  val headCol: Map[H, Col] = headColMap

  val stateRow: Map[Set[HState[T, H, S]], Row] = stateRowMap

  inline def rowShift(state: Row, termHead: Col): Row =
    shifts(state)(termHead)

  inline def rowReduces(state: Row, termHead: Col): List[Row] =
    reduces(state)(termHead)

  inline def rowSparks(state: Row, termHead: Col): List[Row] =
    sparks(state)(termHead)

  val initial: Row = 0
}

object Table {
  def apply[T, H, S](library: HTNLib[T, H, S])
      /* (using TermImpl[T, H, S]) */ : Table[T, H, S] = {

    // val nfa: HandleFinder[T, H, S]

    // val headCol: Map[H, Col] = {
    //   val builder = Map.newBuilder[H, Col]
    //   for (i <- 0 until termHeads.length)
    //     do builder += ((termHeads(i), i))
    //   builder.result
    // }

    // val stateRow: Map[Set[HState[T, H, S]], Row] = {
    //   val builder = Map.newBuilder[Set[HState[T, H, S]], Row]
    //   for (i <- 0 until states.length)
    //     do builder += ((states(i), i))
    //   builder.result
    // }

    new Table(???, ???, ???, ???, ???, ???, ???)
  }
}

// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa

/** Implementation of a [[org.maraist.fa.DFA DFA]] using
 *  [[scala.collection.immutable.IndexedSeq `IndexedSeq`s]] and
 *  `Array`s.
 * @param initialStateIndex Index of the initial state of the automaton.
 * @param finalStateIndices Set of the indices of the final states of the
 * automaton
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on (non-epsilon) transitions of the automaton
 *
 * @group DFA
 */
abstract class AbstractArrayDFA[S,T](
  private val stateSeq: IndexedSeq[S],
  val initialStateIndex: Int, val finalStateIndices: Set[Int],
  private val transitionsSeq: IndexedSeq[T],
  private val transitionsMatrix: Array[Array[Int]]
) extends IndexedDFA[S,T] {
  //println("* * * * *");
  //for (m <- 0 until stateSeq.length) println(m + " :: " + stateSeq(m))
  //println("* * * * *");
  //print("   ")
  //for (n <- 0 until transitionsSeq.length) printf(" %2s", transitionsSeq(n))
  //println()
  //for (m <- 0 until stateSeq.length) {
  //  printf("%2d ", m)
  //  for (n <- 0 until transitionsSeq.length) {
  //    printf(" %2d", transitionsMatrix(m)(n))
  //  }
  //  println()
  //}

  def dump():Unit = {
    println("stateSeq: " + stateSeq)
    println("initialStateIndex: " + initialStateIndex)
    println("finalStateIndices: " + finalStateIndices)
    println("transitionsSeq: " + transitionsSeq)
    println("Transitions by method: " + labels)
    println("Raw transitionsMatrix array: [")
    for(row <- transitionsMatrix) {
      var sep = "  ["
      for(entry <- row) {
        print(sep + Integer.toString(entry))
        sep = ", "
      }
      println("], ")
    }
    println("]")
    println("Transitions by method: [")
    for(from <- stateSeq) {
      var sep = "  [ "
      for(trans <- transitionsSeq) {
        print(sep + transition(from, trans))
        sep = ", "
      }
      println(" ], ")
    }
    println("]")
  }
  def size: Int = stateSeq.length
  def states: IndexedSeq[S] = stateSeq
  def labels: IndexedSeq[T] = transitionsSeq
  /** Returns a transition label by index */
  def labelIndex(t:T):Int = transitionsSeq.indexOf(t)
  def label(i:Int):T = transitionsSeq(i)
  /** Returns a state by index */
  def state(i:Int):S = stateSeq(i)
  /** Returns the index of a state */
  def indexOf(s:S):Int = stateSeq.indexOf(s)
  def initialState: S = stateSeq(initialStateIndex)
  def finalStates: Set[S] = finalStateIndices.map(stateSeq)
  def isState(s:S):Boolean = stateSeq.contains(s)
  def isInitialState(s:S):Boolean = stateSeq(initialStateIndex).equals(s)
  def isFinalState(s:S):Boolean = {
    val si = stateSeq.indexOf(s)
    finalStateIndices.contains(si)
  }

  import scala.util.control.NonLocalReturns.*
  override def accepts(ts:Seq[T]):Boolean = returning {
    var current = initialStateIndex
    for(t <- ts) {
      val ti = transitionsSeq.indexOf(t)
      current = transitionsMatrix(current)(ti)
      if (current<0) throwReturn(false);
    }
    finalStateIndices.contains(current)
  }

  def transition(s:S, t:T):Option[S] = {
    val fromIdx:Int = stateSeq.indexOf(s)
    val labelIdx:Int = transitionsSeq.indexOf(t)
    val toIdx:Option[Int] = transitionIndex(fromIdx,labelIdx)
    toIdx.map(stateSeq)
  }

  /** Returns the index of the state, if any, which is the target of a
   * transition from the state numbered `si` via the transition
   * numbered `ti`
   */
  def transitionIndex(si:Int, ti:Int):Option[Int] = {
    if (si > -1) {
      if (ti > -1) {
        val si2 = transitionsMatrix(si)(ti)
        //println("** " + si + "/" + ti + " --> " + si2)
        if (si2 > -1) Some(si2) else None
      } else None
    } else None
  }
}

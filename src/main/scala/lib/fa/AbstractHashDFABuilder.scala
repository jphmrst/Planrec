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
import scala.collection.mutable.{Builder,Growable,HashMap,HashSet}
import org.maraist.fa.Builders.DFAelements

/** Implementation of [[org.maraist.fa.DFABuilder DFABuilder]] using
 *  [[scala.collection.mutable.HashSet `HashSet`s]] and
 *  [[scala.collection.mutable.HashMap `HashMap`s]].
 * @param initialState The initial state of the automaton.  This builder must
 * be instantiated with one state, initially taken to be the initial state.
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on (non-epsilon) transitions of the automaton
 */
abstract class AbstractHashDFABuilder[S,T, ThisDFA <: AbstractArrayDFA[S,T]](initialState: S)
    extends SingleInitialStateMixin[S,T](initialState)
      with DFABuilder[S,T, ThisDFA]
      with StateHashBuilderTrait[S,T] with FinalStateSetHashBuilderTrait[S,T] {
  private val transitionsMap = new HashMap[S,HashMap[T,S]]

  private[fa] def deleteTransitionsFrom(s:S) = {
    transitionsMap -= s
    for(lmap <- transitionsMap.valuesIterator)
      for(v <- for(v <- lmap.keysIterator if (lmap(v).equals(s))) yield v)
        lmap -= v
  }

  def addTransition(s1:S, t:T, s2:S):Unit = {
    allStates += s1
    allStates += s2
    if (!transitionsMap.contains(s1)) {
      transitionsMap += (s1 -> new HashMap[T,S])
    }
    transitionsMap(s1) += (t -> s2)
  }

  def removeTransition(s1:S, t:T):Unit =
    if (transitionsMap.contains(s1))
      transitionsMap(s1) -= t

  def transition(s:S, t:T):Option[S] = {
    if (transitionsMap.contains(s)) {
      val sub:HashMap[T,S] = transitionsMap(s)
      sub.get(t)
    } else None
  }

  def labels: Set[T] = {
    val result = new HashSet[T]()
    for(map <- transitionsMap.valuesIterator)
      for(t <- map.keysIterator)
        result += t
    result.toSet
  }

  import scala.util.control.NonLocalReturns.*
  def accepts(ts:Seq[T]): Boolean = returning {
    var current:S = initialState
    for(t <- ts) {
      transition(current, t) match {
        case Some(s) => { current = s }
        case None => throwReturn(false)
      }
    }
    isFinalState(current)
  }

  /** @deprecated */
  def toDFA: ThisDFA = result()

  def result(): ThisDFA = {
    val statesSeq: IndexedSeq[S] = IndexedSeq.from(allStates)
    val transitionsSeq: IndexedSeq[T] = IndexedSeq.from(labels)
    val initialIdx: Int = statesSeq.indexOf(initialState)
    val finalStateIndices: HashSet[Int] = new HashSet[Int]
    for(s <- finalStatesSet) finalStateIndices += statesSeq.indexOf(s)

    val idxLabels:Array[Array[Int]] =
      Array.ofDim[Int](statesSeq.length, transitionsSeq.length)
    for(si <- 0 until statesSeq.length) {
      val s:S = statesSeq(si)
      for(ti <- 0 until transitionsSeq.length) {
        val t:T = transitionsSeq(ti)
        if (transitionsMap.contains(s)) {
          if (transitionsMap(s).contains(t)) {
            idxLabels(si)(ti) = statesSeq.indexOf(transitionsMap(s)(t))
          } else {
            idxLabels(si)(ti) = -1
          }
        } else {
          idxLabels(si)(ti) = -1
        }
      }
    }

    assembleDFA(statesSeq, initialIdx, finalStateIndices, transitionsSeq,
                idxLabels)
  }

  protected def assembleDFA(statesSeq: IndexedSeq[S], initialIdx: Int,
                            finalStateIndices: HashSet[Int],
                            transitionsSeq: IndexedSeq[T],
                            idxLabels: Array[Array[Int]]): ThisDFA
}

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
import scala.collection.mutable.{HashMap}

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
class ArrayPFA[S,T](private val stateSeq: IndexedSeq[S],
                    val initialProbs: Array[Double],
                    val finalProbs: Array[Double],
                    private val transitionsSeq: IndexedSeq[T],
                    private val transitionsMatrix: Array[Array[Array[Double]]],
                    private val eTransitionsMatrix: Array[Array[Double]]
                  ) extends IndexedPFA[S,T] {

  def size: Int = stateSeq.length
  def states: IndexedSeq[S] = stateSeq
  def labels: IndexedSeq[T] = transitionsSeq
  /** Returns a transition label by index */
  def labelIndex(t:T): Int = transitionsSeq.indexOf(t)
  def label(i:Int):T = transitionsSeq(i)
  /** Returns a state by index */
  def state(i:Int):S = stateSeq(i)
  /** Returns the index of a state */
  def indexOf(s:S):Int = stateSeq.indexOf(s)
  def isState(s:S):Boolean = stateSeq.contains(s)
  def initialStateIndexProb(s:Int):Double = initialProbs(s)
  def finalStateIndexProb(s:Int):Double = finalProbs(s)

  import scala.util.control.NonLocalReturns.*
  override def accepts(ts:Seq[T]): Double = returning {
    var current = List[(Double,Int)]()
    for(s:Int <- 0 until size) {
      val thisProb = initialStateIndexProb(s)
      if (thisProb > 0.0) { current = (thisProb,s) :: current }
    }

    for(t <- ts) {
      val ti = transitionsSeq.indexOf(t)
      val oldCurrent = current
      current = List[(Double,Int)]()
      for ((p0,s0) <- oldCurrent) {
        for ((s1,p1) <- transitionIndex(s0,ti)) {
          if (p1>0.0) {
            current = (p0*p1,s1) :: current
          }
        }
      }
      if (current.size == 0) throwReturn(0.0)
    }

    var result:Double = 0.0
    for ((p,s) <- current) { result += p*finalStateIndexProb(s) }
    result
  }

  def eTransitionIndex(s0:Int, s1:Int): Double = eTransitionsMatrix(s0)(s1)

  def transitionIndex(fromIdx:Int, labelIdx:Int, toIdx:Int):Double = {
    transitionsMatrix(fromIdx)(labelIdx)(toIdx)
  }

  def transition(s0:S, t:T):Map[S,Double] = {
    val fromIdx:Int = stateSeq.indexOf(s0)
    val labelIdx:Int = transitionsSeq.indexOf(t)
    val idxMap = transitionIndex(fromIdx,labelIdx)
    val result = new HashMap[S,Double]
    for ((idx,prob) <- idxMap) {
      result += (stateSeq(idx) -> prob)
    }
    result.toMap
  }

  def transitionIndex(s0:Int, t:Int):Map[Int,Double] = {
    val result = new HashMap[Int,Double]
    val arr = transitionsMatrix(s0)(t)
    for (idx <- 0 to arr.length) {
      if (arr(idx)>0.0) { result += (idx -> arr(idx)) }
    }
    result.toMap
  }
}

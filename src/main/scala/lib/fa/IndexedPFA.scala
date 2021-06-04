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
import scala.collection.mutable.{HashSet}
import org.maraist.graphviz.Graphable
import org.maraist.graphviz.NodeLabeling
import org.maraist.graphviz.TransitionLabeling

/** Trait of the basic usage operations on a DFA.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 */
trait IndexedPFA[S,T] extends IndexedAutomaton[S,T] with PFA[S,T] {
  def transitionIndex(fromIdx:Int, labelIdx:Int, toIdx:Int):Double
  def transitionIndex(s0:Int, t:Int):Map[Int,Double]
  def eTransitionIndex(s0:Int, s1:Int):Double
  def initialStateIndexProb(s:Int):Double
  def finalStateIndexProb(s:Int):Double

  def initialStateIndices: Set[Int] = {
    val result = new HashSet[Int]
    for(si <- 0 until size) {
      if (initialStateIndexProb(si) > 0.0) { result += si }
    }
    result.toSet
  }

  def finalStateIndices: Set[Int] = {
    val result = new HashSet[Int]
    for(si <- 0 until size) {
      if (finalStateIndexProb(si) > 0.0) { result += si }
    }
    result.toSet
  }

  def initialStateProb(s:S):Double = initialStateIndexProb(indexOf(s))
  def finalStateProb(s:S):Double = finalStateIndexProb(indexOf(s))

  def transition(s0:S, t:T, s1:S):Double = {
    val fromIdx:Int = indexOf(s0)
    val labelIdx:Int = labelIndex(t)
    val toIdx:Int = indexOf(s1)
    transitionIndex(fromIdx, labelIdx, toIdx)
  }

  def eTransition(s0: S,s1: S):Double = {
    val fromIdx:Int = indexOf(s0)
    val toIdx:Int = indexOf(s1)
    eTransitionIndex(fromIdx, toIdx)
  }

  /** Traverse the structure of this PFA, states first, then transitions */
  override def traverse(trav:PFAtraverser[S,T]) = {
    trav.init()
    for(si <- 0 until size) {
      val s = state(si)
      trav.state(si,s,initialStateIndexProb(si),finalStateIndexProb(si))
    }
    trav.postState()
    for(si0 <- 0 until size) {
      val s0 = state(si0)
      for(ti <- 0 until labels.size) {
        val t = label(ti)
        var absent=true
        for(si1 <- 0 until size) {
          val s1 = state(si1)

          val prob = transitionIndex(si0,ti,si1)
          if (prob>0.0) {
            //println("        echo " + si0 + " " + ti + " " + si1)
            trav.presentEdge(si0, s0, ti, t, si1, s1, prob)
            absent=false
          } else {
            trav.absentEdge(si0, s0, ti, t, si1, s1)
          }
          //println("  end traversing")
        }

        if (absent) {
          trav.absentEdge(si0, s0, ti, t)
        }
      }
    }
    trav.finish()
  }
}

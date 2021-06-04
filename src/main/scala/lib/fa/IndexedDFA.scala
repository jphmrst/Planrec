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
import org.maraist.graphviz.Graphable
import org.maraist.graphviz.NodeLabeling
import org.maraist.graphviz.TransitionLabeling

/** Trait of the basic usage operations on a DFA.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 */
trait IndexedDFA[S,T] extends DFA[S,T] with IndexedAutomaton[S,T] {
  def initialStateIndex:Int
  def initialStateIndices:Set[Int] = Set(initialStateIndex)

  def transitionIndex(si:Int, ti:Int): Option[Int]

  /** Traverse the structure of this DFA, states first, then transitions */
  override def traverse(trav:Traverser) = {
    trav.init(states.size, labels.size)
    for(si <- 0 until size) {
      val s = state(si)
      trav.state(si,s,si==initialStateIndex,isFinalState(s))
    }
    trav.postState()
    for(si0 <- 0 until size) {
      val s0 = state(si0)
      traverseEdgesFrom(si0, s0, states, labels, trav)
    }
    trav.finish()
  }

  override protected def traverseEdgesFrom(si0:Int, s0:S,
                                           stateList:IndexedSeq[S],
                                           theLabels:IndexedSeq[T],
                                           trav:Traverser) = {
    for(ti <- 0 until labels.size) {
      val t = label(ti)
      val toIndex = transitionIndex(si0,ti)
      //println("  traversing " + si0 + "/" + ti + " --> " + toIndex)
      toIndex match {
        case Some(si1) => {
          //println("        echo " + si0 + " " + ti + " " + si1)
          trav.presentEdge(si0, s0, ti, t, si1, state(si1))
        }
        case None => {
          trav.absentEdge(si0, s0, ti, t)
        }
      }
      //println("  end traversing")
    }
  }
}

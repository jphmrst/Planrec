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
import scala.collection.mutable.{HashMap,HashSet}
import org.maraist.graphviz.Graphable
import org.maraist.graphviz.NodeLabeling
import org.maraist.graphviz.TransitionLabeling
import org.maraist.fa.Builders.{HasBuilder,PFAelements}

/** Trait of the basic usage operations on a PFA.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 *
 * @group PFA
 */
trait PFA[S,T] extends Automaton[S,T] with Graphable[S,T] {

  /** Returns the probability that `s` is an initial state of this automaton.
   */
  def initialStateProb(s:S): Double

  /** Returns the probability that `s` is a final state of this automaton. */
  def finalStateProb(s:S): Double

  /** Returns `true` if `s` has a non-zero chance of being a final state of
   *  the automaton
   */
  def isFinalState(s:S): Boolean = finalStateProb(s)>0.0

  /** {@inheritDoc} For PFAs, this method returns the states which map via
   *  {@link org.maraist.fa.PFA#finalStateProb} to a non-zero probability.
   */
  def finalStates: Set[S] = {
    val result = new HashSet[S]
    for(m <- states) { if (finalStateProb(m)>0.0) { result += m } }
    result.toSet
  }

  /** Returns `true` if `s` has a non-zero chance of being an initial state
   *  of the automaton */
  def isInitialState(s:S): Boolean = initialStateProb(s)>0.0

  /** {@inheritDoc} For PFAs, this method returns the states which map via
   *  {@link org.maraist.fa.PFA#initialStateProb} to a non-zero probability.
   */
  def initialStates: Set[S] = {
    val result = new HashSet[S]
    for(m <- states) { if (initialStateProb(m)>0.0) { result += m } }
    result.toSet
  }

  /** Calculate the probability that the automaton accepts the given sequence
   */
  def accepts(ts:Seq[T]): Double

  /** Returns the states into which the automaton could transition with
   * non-zero probability starting from `s` via a transition labelled `t`.
   *
   * In the resulting map, each resulting state is associated with the
   * probability of that transition taking place.
   */
  def transition(s:S, t:T): Map[S,Double]

  /** Returns the probability that from state s0 and with label t, this state
   * would transition into state s1.
   */
  def transition(s0:S, t:T, s1:S): Double

  /** Returns the probability of a transition state s0 into state s1 without
   *  a labeled event.
   */
  def eTransition(s0:S, s1:S): Double

  override def toString():String = {
    val bld:StringBuilder = new StringBuilder
    for (st <- this.states) {
      if (isInitialState(st)) bld ++= "> " else bld ++= "  "
      bld ++= st.toString() + "\n"
//       for (tr <- labels)
//         transition(st, tr) match {
//           case Some(x) => bld ++= ("  - " + tr + " --> " + x + "\n")
//           case None =>
//         }
    }
    bld.toString()
  }

   /** Traverse the structure of this DFA, states first, then transitions */
   def traverse(trav:PFAtraverser[S,T]) = {
     val stateList = IndexedSeq.from(states)
     val theLabels = IndexedSeq.from(labels)
     trav.init()
     for(si <- 0 until stateList.length) {
       val s = stateList(si)
       trav.state(si,s,initialStateProb(s),finalStateProb(s))
     }
     trav.postState()
     for(si0 <- 0 until stateList.length) {
       val s0 = stateList(si0)

       for(si1 <- 0 until stateList.length) {
         val s1 = stateList(si1)
         val prob = eTransition(s0,s1)
         if (prob>0.0) {
           trav.presentEdge(si0, s0, si1, s1, prob)
         } else {
           trav.absentEpsilonEdge(si0, s0, si1, s1)
         }
       }

       for(ti <- 0 until theLabels.length) {
         val t = theLabels(ti)
         var present=false
         for(si1 <- 0 until stateList.length) {
           val s1 = stateList(si1)
           val prob = transition(s0,t,s1)
           if (prob>0.0) {
             trav.presentEdge(si0, s0, ti, t, si1, s1, prob)
             present=true
           } else {
             trav.absentEdge(si0, s0, ti, t, si1, s1)
           }
         }
         if (!present) {
           trav.absentEdge(si0, s0, ti, t)
         }
       }
     }
     trav.finish()
   }

  /** Internal routine used by {@link #toDOT}.  Subclesses should
   *  override, but still call super.internalsToDOT, to extend the
   *  Graphviz representation of a DFA */
  protected def internalsToDOT(stateList:IndexedSeq[S],
                               sb:StringBuilder,
                               nodeLabeling:NodeLabeling[S] =
                                 this.nodeLabeling,
                               trLabeling:TransitionLabeling[T] =
                                 this.transitionLabeling):Unit = {
    traverse(new PFAdotTraverser[S,T](sb, nodeLabeling, trLabeling,
                                      graphvizOptions))
  }

  /** {@inheritDoc} */
  def toDOT(nodeLabeling:NodeLabeling[S] = this.nodeLabeling,
            transitionLabeling:TransitionLabeling[T] =
              this.transitionLabeling):String = {
    val stateList = IndexedSeq.from(states)
    val sb = new StringBuilder()
    internalsToDOT(stateList,sb,nodeLabeling,transitionLabeling)
    sb.toString()
  }

  private[fa] def getEdgeProbTotals: Map[S, Map[T, Double]] = {
    val totals = new HashMap[S, HashMap[T,Double]]

    traverse(new PFAtraverser[S,T] {
      override def state(i:Int, s:S, init:Double, fin:Double) = {
        totals += ((s, new HashMap[T,Double]()))
      }
      override def presentEdge(fromIndex:Int, fromState:S,
                               labelIndex:Int, label:T,
                               toIndex:Int, toState:S, prob:Double) = {
        val smap = totals(fromState)
        if (!smap.contains(label)) { smap(label) = 0.0 }
        smap(label) += prob
      }
    })

    totals.map({ case (k, vm) => (k, vm.toMap) }).toMap
  }
}

object PFA {
  def newBuilder[S, T, SetType[_], MapType[_,_]](initialState: S)(
    using impl: HasBuilder[SetType, MapType, PFAelements, PFA]
  ) = impl.build[S,T]()
}

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
import scala.collection.mutable.{Builder,Growable}
import org.maraist.fa.Builders.*

/** Builders for probabilistic finite automata (PFAs)
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  */
trait PFABuilder[S,T]
    extends PFA[S,T]
    with Builder[PFAelements[S,T], PFA[S,T]]
    with Growable[PFAelements[S, T]] {
  /** Adds a state to the automaton */
  def addState(s:S):Unit
  /** Removes a state from the automaton */
  def removeState(s:S):Unit

  /** Adds a state to the automaton */
  def addInitialState(s:S, prob:Double):Unit
  /** Removes a state from the automaton */
  def removeInitialState(s:S):Unit

  /** Adds a state to the automaton */
  def addFinalState(s:S, prob:Double):Unit
  /** Removes a state from the automaton */
  def removeFinalState(s:S):Unit

  /** Adds a transition labelled `t` from `s1` to `s2`, removing any
   *  previous transition labelled `t` from `s1` to `s2`.
   */
  def addTransition(s1:S, t:T, s2:S, prob:Double): Unit
  /** Removes any transition labelled `t` from `s1` to `s2` */
  def removeTransition(s1:S, t:T, s2:S): Unit

  /** Adds an epsilon transition from `s1` to `s2`, removing any
   *  previous epsilon transition from `s1` to `s2`.
   */
  def addETransition(s1:S, s2:S, prob:Double): Unit
  /** Removes any epsilon transition from `s1` to `s2` */
  def removeETransition(s1:S, s2:S): Unit

  /** Returns the (possibly immutable) [[org.maraist.fa.PFA PFA]]
    * described to this builder */
  type ThisPFA <: PFA[S,T]
  def toPFA: ThisPFA

  def removeEpsilonTransitions:Unit = new EpsilonRemover(this).run()

  override def addOne(builder: PFAelements[S,T]): this.type = {
    builder match {
      case AddState(s) => addState(s)
      case RemoveState(state) => removeState(state)
      case AddProbFinalState(state, prob) => addFinalState(state, prob)
      case RemoveFinalState(state) => removeFinalState(state)
      case AddProbTransition(state1, trans, state2, prob) =>
        addTransition(state1, trans, state2, prob)
      case RemoveTransition(state1, trans, state2) =>
        removeTransition(state1, trans, state2)
      case SetInitialState(state) => SetInitialState(state)
      case AddProbETransition(state1, state2, prob) =>
        addETransition(state1, state2, prob)
      case RemoveProbETransition(state1, state2, prob) =>
        removeETransition(state1, state2)
    }
    this
  }

  /** This {@link scala.collection.mutable.Builder Builder} method
    * is not implemented at this time.
    */
  def clear(): Unit = throw new UnsupportedOperationException()
}

/** Implementation of de la Higurera's Algorithm 5.8 for eliminating
 *  epsilon-transitions.
 */
private[fa] class EpsilonRemover[S,T](val builder:PFABuilder[S,T]) {
  val statesSeq = Seq.from(builder.states)

  def run():Unit = {
    var change:ETransFinder = Start()
    while (change.more) {
      change = findNextEpsilonTransition
      change.apply()
    }
  }

  protected sealed abstract class ETransFinder {
    def apply():Unit
    def more:Boolean
  }
  protected case class ELoop(val stateIndex:Int,
                             val loopProb:Double) extends ETransFinder {
    def more:Boolean = true
    def apply():Unit = {
      val state:S = statesSeq(stateIndex)
      val factor = 1.0 / (1.0 - loopProb)

      for(t <- builder.labels) {
        for(si1 <- 0 until statesSeq.length) {
          val s1:S = statesSeq(si1)
          val prob = builder.transition(state,t,s1)
          if (prob>0.0) {
            builder.addTransition(state,t,s1, prob*factor)
          }
        }
      }

      for(si1 <- 0 until statesSeq.length) {
        if (si1 != state) {
          val s1:S = statesSeq(si1)
          val prob = builder.eTransition(state,s1)
          if (prob>0.0) {
            builder.addETransition(state,s1, prob*factor)
          }
        }
      }

      val fProb = builder.finalStateProb(state)
      builder.addFinalState(state, fProb*factor)

      builder.removeETransition(state,state)
    }
  }
  protected case class ENonloop(q:Int, m:Int,
                                prob:Double) extends ETransFinder {
    def more:Boolean = true
    def apply():Unit = {
      val qState:S = statesSeq(q)
      val mState:S = statesSeq(m)
      // val factor = 1.0 / (1.0 - loopProb)

      for(n <- m + 1 until statesSeq.length) {
        val nState:S = statesSeq(n)
        var linkProb = builder.eTransition(qState,nState)
        var incr = prob * builder.eTransition(mState,nState)
        if (linkProb>0 || incr>0)
          builder.addETransition(qState,nState, linkProb + incr)
      }

      for(t <- builder.labels) {
        for(n <- 0 until statesSeq.length) {
          val nState:S = statesSeq(n)
          var linkProb = builder.transition(qState,t,nState)
          var incr = prob * builder.transition(mState,t,nState)
          if (linkProb>0 || incr>0)
            builder.addTransition(qState,t,nState, linkProb + incr)
          }
      }

      val fProb = builder.finalStateProb(qState)
      val fIncr = prob * builder.finalStateProb(mState)
      if (fProb>0 || fIncr>0)
        builder.addFinalState(qState, fProb + fIncr)

      builder.removeETransition(qState, mState)
    }
  }
  protected case class NoETrans() extends ETransFinder {
    def more:Boolean = false
    def apply():Unit = { }
  }
  protected case class Start() extends ETransFinder {
    def more:Boolean = true
    def apply():Unit = { }
  }

  import scala.util.control.NonLocalReturns.*
  protected def findNextEpsilonTransition: ETransFinder =
    returning[ETransFinder] {
      for(si <- 0 until statesSeq.length) {
        val s:S = statesSeq(si)
        val p = builder.eTransition(s,s)
        if (p > 0.0)
          throwReturn[ETransFinder](ELoop(si,p))
      }

      for(si0 <- 0 until statesSeq.length) {
        val s0:S = statesSeq(si0)

        for(si1 <- 0 until statesSeq.length) {
          val s1:S = statesSeq(si1)
          val p = builder.eTransition(s0,s1)
          if (p > 0.0)
            throwReturn[ETransFinder](ENonloop(si0,si1,p))
        }
      }

      NoETrans()
    }
}

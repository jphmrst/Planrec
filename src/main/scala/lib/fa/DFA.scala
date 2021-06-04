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
import org.maraist.graphviz.{Graphable,GraphvizOptions,
                             NodeLabeling,TransitionLabeling}
import org.maraist.fa.Builders.{HasBuilderWithInit,DFAelements}

/** Trait of the basic usage operations on a DFA.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 *
 * @group DFA
 */
trait DFA[S,T] extends Automaton[S,T] with Graphable[S,T] {
  type Traverser <: DFAtraverser[S,T]

  /** The initial state of the automaton */
  def initialState: S
  /** {@inheritDoc} For DFAs, this method returns a singleton set containing
   * the result of {@link org.maraist.fa.DFA#initialState initialState}.
   */
  def initialStates: Set[S] = Set(initialState)
  /** Returns the state, if any, into which the automaton could
   * transition starting from `s` via a transition labelled `t`.
   */
  def transition(s:S, t:T): Option[S]
  /** Check whether the automaton accepts the given sequence */
  def accepts(ts:Seq[T]): Boolean
  override def toString():String = {
    val bld:StringBuilder = new StringBuilder
    for (st <- states) {
      if (isInitialState(st)) bld ++= "> " else bld ++= "  "
      bld ++= st.toString() + "\n"
      for (tr <- labels)
        transition(st, tr) match {
          case Some(x) => bld ++= ("  - " + tr + " --> " + x + "\n")
          case None =>
        }
    }
    bld.toString()
  }

  /** Traverse the structure of this DFA, states first, then transitions */
  def traverse(trav:Traverser) = {
    val stateList = IndexedSeq.from(states)
    val theLabels = IndexedSeq.from(labels)
    trav.init(stateList.size, theLabels.size)
    for(si <- 0 until stateList.length) {
      val s = stateList(si)
      trav.state(si,s,isInitialState(s),isFinalState(s))
    }
    trav.postState()
    for(si0 <- 0 until stateList.length) {
      val s0 = stateList(si0)
      traverseEdgesFrom(si0, s0, stateList, theLabels, trav)
    }
    trav.finish()
  }

  protected def traverseEdgesFrom(si0:Int, s0:S, stateList:IndexedSeq[S],
                                  theLabels:IndexedSeq[T], trav:Traverser) = {
    for(ti <- 0 until theLabels.length) {
      val t = theLabels(ti)
      transition(s0,t) match {
        case Some(s1) =>
          trav.presentEdge(si0, s0, ti, t, stateList.indexOf(s1), s1)
        case None => trav.absentEdge(si0, s0, ti, t)
      }
    }
  }

  protected def dotTraverser(sb:StringBuilder,
                             stateList:IndexedSeq[S]): Traverser

  /** Internal routine used by {@link #toDOT}.  Subclesses should
   *  override, but still call super.internalsToDOT, to extend the
   *  Graphviz representation of a DFA */
  protected def internalsToDOT(stateList:IndexedSeq[S],
                               sb:StringBuilder,
                               nodeLabeling:NodeLabeling[S] =
                                 this.nodeLabeling,
                               trLabeling:TransitionLabeling[T] =
                                 this.transitionLabeling):Unit = {

    // Initial state
    sb ++= "\tinit [shape=none, margin=0, label=\"\"];\n"

    traverse(dotTraverser(sb, stateList))
    for(si0 <- 0 until stateList.length) {
      val s0 = stateList(si0)
      for(t <- labels) {
        transition(s0,t) match {
          case Some(s1) => {
          }
          case None => { }
        }
      }
    }
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
}

object DFA {
  def newBuilder[S, T, SetType[_], MapType[_,_]](initialState: S)(
    using impl: HasBuilderWithInit[SetType, MapType, DFAelements, DFA]
  ) = impl.build[S,T](initialState)
}

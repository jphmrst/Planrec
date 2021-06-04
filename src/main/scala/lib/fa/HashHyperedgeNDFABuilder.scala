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
import scala.collection.mutable.{Builder,HashSet,HashMap}
import org.maraist.fa.Builders.*

class HashHyperedgeNDFABuilder[S,T]
    extends AbstractHashNDFABuilder[
      S,T,ArrayHyperedgeDFA[Set[S],T], ArrayHyperedgeNDFA[S,T]
    ] with HyperedgeNDFABuilder[
      S,T,ArrayHyperedgeDFA[Set[S],T], ArrayHyperedgeNDFA[S,T]
    ] with Builder[
      HyperedgeNDFAelements[S,T], HyperedgeNDFA[S,T,ArrayHyperedgeDFA[Set[S],T]]]
{
  val hyperedgeMap: HashMap[S,HashSet[Set[S]]] = new HashMap[S,HashSet[Set[S]]]
  def eHyperedgeTargets(s:S): Set[Set[S]] = hyperedgeMap.get(s) match {
    case Some(hashSet) => hashSet.toSet
    case None => Set.empty[Set[S]]
  }
  def addEHyperedge(s:S, ss:Set[S]): Unit = {
    if (!hyperedgeMap.contains(s))  hyperedgeMap += (s -> new HashSet[Set[S]])
    hyperedgeMap(s) += ss
  }
  protected def assembleNDFA(statesSeq:IndexedSeq[S], initials:Set[Int],
                             finals:Set[Int], transitionsSeq: IndexedSeq[T],
                             labelsArray:Array[Array[HashSet[Int]]],
                             epsilonsArray:Array[HashSet[Int]]): ArrayHyperedgeNDFA[S,T] = {
    val hyperedgeIndexMap: HashMap[Int,Set[Set[Int]]] =
      new HashMap[Int,Set[Set[Int]]]
    for((startNode,targetSets) <- hyperedgeMap) {
      val startNodeIndex = statesSeq.indexOf(startNode)
      val thisSet = new HashSet[Set[Int]]
      for(targetSet <- targetSets)
        thisSet += targetSet.map(statesSeq.indexOf(_))
      hyperedgeIndexMap(startNodeIndex) = thisSet.toSet
    }
    new ArrayHyperedgeNDFA[S,T](
      statesSeq, initials, finals, transitionsSeq, labelsArray,
      epsilonsArray.map(_.toSet),
      hyperedgeIndexMap.toMap)
  }

  override def toDFA: ArrayHyperedgeDFA[Set[S],T] = this.toNDFA.toDFA

  /** Dispatch steps for a Builder-pattern implementation.  */
  override def addOne(builder: HyperedgeNDFAelements[S,T]): this.type = {
    builder match {
      case AddState(s) => addState(s)
      case RemoveState(state) => removeState(state)
      case AddFinalState(state) => addFinalState(state)
      case RemoveFinalState(state) => removeFinalState(state)
      case AddTransition(state1, trans, state2) =>
        addTransition(state1, trans, state2)
      case RemoveTransition(state1, trans, state2) =>
        removeTransition(state1, trans, state2)
      case AddInitialState(state) => addInitialState(state)
      case RemoveInitialState(state) => removeInitialState(state)
      case AddETransition(state1, state2) => addETransition(state1, state2)
      case RemoveETransition(state1, state2) => removeETransition(state1, state2)
      case AddEHyperedge(fromState, toStates) => addEHyperedge(fromState, toStates)
    }
    this
  }
}

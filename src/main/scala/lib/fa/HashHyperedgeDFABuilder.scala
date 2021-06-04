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

class HashHyperedgeDFABuilder[S, T](initialState: S)
    extends AbstractHashDFABuilder[S, T, ArrayHyperedgeDFA[S, T]](initialState)
    with HyperedgeDFABuilder[S, T, ArrayHyperedgeDFA[S, T]]
    with Builder[HyperedgeDFAelements[S, T], HyperedgeDFA[S, T]]{
  val hyperedgeMap: HashMap[S,HashSet[Set[S]]] = new HashMap[S,HashSet[Set[S]]]
  def eHyperedgeTargets(s:S): Set[Set[S]] = hyperedgeMap.get(s) match {
    case Some(set) => set.toSet
    case None => Set.empty[Set[S]]
  }
  def addEHyperedge(s:S, ss:Set[S]): Unit = {
    if (!hyperedgeMap.contains(s))  hyperedgeMap += (s -> new HashSet[Set[S]])
    hyperedgeMap(s) += ss
  }

  type Traverser = HyperedgeDFAtraverser[S,T]
  protected def dotTraverser(sb:StringBuilder,stateList:IndexedSeq[S]) =
    new DotTraverseHyperedgeDFA[S,T](graphvizOptions, sb, nodeLabeling,
                                     transitionLabeling, stateList,
                                     initialState)

  protected def assembleDFA(statesSeq: IndexedSeq[S],
                            initialIdx: Int,
                            finalStateIndices: HashSet[Int],
                            transitionsSeq: IndexedSeq[T],
                            idxLabels: Array[Array[Int]]): ArrayHyperedgeDFA[S,T] = {
    val hyperedgeIndicesMap = new HashMap[Int,HashSet[Set[Int]]]
    for((node,nodeSets) <- hyperedgeMap) {
      val nodeIdx = statesSeq.indexOf(node)
      val thisSet = new HashSet[Set[Int]]
      for(nodeSet <- nodeSets)
        thisSet += nodeSet.map(statesSeq.indexOf(_))
      hyperedgeIndicesMap(nodeIdx) = thisSet
    }
    new ArrayHyperedgeDFA[S,T](
      statesSeq, initialIdx, finalStateIndices.toSet,
      transitionsSeq, idxLabels,
      hyperedgeIndicesMap.map({case (k, v) => (k, v.toSet)}).toMap)
  }

  /** Primary {@link scala.collection.mutable.Builder Builder} method
    * implementation.
    */
  override def addOne(builder: HyperedgeDFAelements[S, T]): this.type = {
    builder match {
      case AddState(s) => addState(s)
      case RemoveState(state) => removeState(state)
      case AddFinalState(state) => addFinalState(state)
      case RemoveFinalState(state) => removeFinalState(state)
      case AddTransition(state1, trans, state2) =>
        addTransition(state1, trans, state2)
      case RemoveTransition(state, trans, state2) => removeTransition(state, trans)
      case SetInitialState(state) => setInitialState(state)
      case AddEHyperedge(state, toStates) => addEHyperedge(state, toStates)
    }
    this
  }
}

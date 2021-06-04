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
import scala.collection.mutable.HashMap

class ArrayHyperedgeDFA[S,T](stateSeq: IndexedSeq[S],
                             initialStateIndex: Int,
                             finalStateIndices: Set[Int],
                             transitionsSeq: IndexedSeq[T],
                             labels: Array[Array[Int]],
                             protected val
                                 hyperedgeIndexMap: Map[Int,Set[Set[Int]]])
extends AbstractArrayDFA[S,T](stateSeq,initialStateIndex,finalStateIndices,
                              transitionsSeq,labels)
with IndexedHyperedgeDFA[S,T] {
  type Traverser = HyperedgeDFAtraverser[S,T]
  protected def dotTraverser(sb:StringBuilder,stateList:IndexedSeq[S]) =
    new DotTraverseHyperedgeDFA[S,T](graphvizOptions, sb, nodeLabeling,
                                     transitionLabeling, stateList,
                                     initialState)

  protected val hyperedgeMap = new HashMap[S,Set[Set[S]]]
  for((si,iss) <- hyperedgeIndexMap)
    hyperedgeMap(stateSeq(si)) = iss.map({ set => set.map(stateSeq(_)) })

  def eHyperedgeTargetIndices(si:Int): Set[Set[Int]] = hyperedgeIndexMap(si)
  def eHyperedgeTargets(s:S): Set[Set[S]] = hyperedgeMap.get(s) match {
    case Some(set) => set
    case None => Set.empty[Set[S]]
  }
}

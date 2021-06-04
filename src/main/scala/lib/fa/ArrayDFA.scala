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
class ArrayDFA[S,T](stateSeq: IndexedSeq[S],
                    initialStateIndex: Int, finalStateIndices: Set[Int],
                    transitionsSeq: IndexedSeq[T],
                    transitionsMatrix: Array[Array[Int]]
                  )
extends AbstractArrayDFA[S,T](stateSeq, initialStateIndex, finalStateIndices,
                              transitionsSeq, transitionsMatrix) {
  type Traverser = DFAtraverser[S,T]
  protected def dotTraverser(sb:StringBuilder,stateList:IndexedSeq[S]) =
    new DotTraverseDFA[S,T](graphvizOptions, sb, nodeLabeling,
                            transitionLabeling, stateList, initialState)
}

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

/**
 * Methods for traversing the structure of a {@link org.maraist.fa.DFA DFA}.
 * Use with the {@link org.maraist.fa.DFA#traverse DFA.traverse} method. By
 * default, all methods are empty.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 */
trait DFAtraverser[-S,-T] {
  /** Called at the beginning of traversal, before any other methods. */
  def init(states:Int, labels:Int): Unit
  /** Called once for each state in the {@link org.maraist.fa.DFA DFA}. */
  def state(index:Int, state:S, isInitial:Boolean, isFinal:Boolean): Unit
  /**
   * Called after the last call to {@link org.maraist.fa.DFAtraverser#state
   * state}, but before any calls to
   * {@link org.maraist.fa.DFAtraverser#presentEdge presentEdge}
   * or {@link org.maraist.fa.DFAtraverser#absentEdge absentEdge}.
   */
  def postState(): Unit
  /** Called for each transition between states in the DFA. */
  def presentEdge(fromIndex:Int, fromState:S, labelIndex:Int, label:T,
                  toIndex:Int, toState:S): Unit
  /** Called for each state/label pair for which there is no target state. */
  def absentEdge(fromIndex:Int, fromState:S, labelIndex:Int, label:T): Unit
  /** Called last among the methods of this trait for any traversal. */
  def finish(): Unit
}


/**
 * Methods for traversing the structure of a
 * {@link org.maraist.fa.HyperedgeDFA HyperedgeDFA}. Use with the
 * {@link org.maraist.fa.HyperedgeDFA#traverse HyperedgeDFA.traverse} method.
 * By default, all methods are empty.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 */
trait HyperedgeDFAtraverser[S,-T] extends DFAtraverser[S,T] {
  def eHyperedge(sourceIndex:Int, source:S, targets:Set[S]): Unit
}

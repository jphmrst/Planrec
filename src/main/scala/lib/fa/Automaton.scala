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

/** Trait of the basic usage operations on an automaton with states and labels.
 *
 *  Makes no assumptions about the nature of the transitions between states.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 *
 * @group DFA
 */
trait Automaton[S,T] {
  /** Number of states in the automaton */
  def size: Int
  /** Set of automaton states */
  def states: Iterable[S]
  /** Set of automaton transition labels */
  def labels: Iterable[T]
  /** Set of the initial states of the automaton */
  def initialStates: Set[S]
  /** Set of the accept states of the automaton */
  def finalStates: Set[S]
  /** Returns `true` if `s` is used as a state */
  def isState(s:S): Boolean
  /** Returns `true` if `s` is an initial state of the automaton */
  def isInitialState(s:S): Boolean
  /** Returns `true` if `s` is a final state of the automaton */
  def isFinalState(s:S): Boolean
}


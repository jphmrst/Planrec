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
import scala.collection.mutable.Builder
import org.maraist.fa.Builders.DFAelements

/** Builders for deterministic finite automata (DFAs)
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  */
trait DFABuilder[S, T, +ThisDFA <: DFA[S,T]] extends DFA[S,T] {

  /** Returns the (possibly immutable) [[org.maraist.fa.DFA DFA]]
    * described to this builder */
  def result(): ThisDFA

  /** Adds a state to the automaton */
  def addState(s:S):Unit
  /** Removes a state from the automaton */
  def removeState(s:S):Unit

  /** Sets the initial state of the automaton */
  def setInitialState(s:S):Unit
  /** Adds a final state to the automaton */
  def addFinalState(s:S):Unit
  /** Causes a state not to be considered a final state, but does
   *  ''not'' remove it from the automaton */
  def removeFinalState(s:S):Unit

  /** Adds a transition labelled `t` from `s1` to `s2`, removing any
   *  previous transition labelled `t` from `s1`.
   */
  def addTransition(s1:S, t:T, s2:S): Unit
  /** Removes any transition labelled `t` from `s1` to `s2` */
  def removeTransition(s1:S, t:T): Unit

  /** This {@link scala.collection.mutable.Builder Builder} method
    * is not implemented at this time.
    */
  def clear(): Unit = throw new UnsupportedOperationException()

  /** @deprecated Use {@link #result} */
  def toDFA: ThisDFA
}

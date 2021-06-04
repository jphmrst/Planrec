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
import org.maraist.fa.Builders.NDFAelements

/** Builders for nondeterministic finite automata (NDFAs)
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  */
trait NDFABuilder[S, T, +ThisDFA <: IndexedDFA[Set[S],T],
                  +ThisNDFA <: NDFA[S,T,ThisDFA]]
    extends NDFA[S,T,ThisDFA]
    with NDFABuilderWriter[S,T] {

  /** Returns the (possibly immutable) [[org.maraist.fa.NDFA NDFA]]
    * described to this builder */
  def toNDFA: ThisNDFA
}

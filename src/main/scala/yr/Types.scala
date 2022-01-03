// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr
import scala.collection.mutable.Queue
import org.maraist.graphviz.Graphable
import org.maraist.fa.{
  EdgeAnnotatedNFA, EdgeAnnotatedNFABuilder, EdgeAnnotatedDFA}
import org.maraist.fa.util
import org.maraist.planrec.rules.{All,One,Act,TriggerHint,TriggerMatchIndex}
import org.maraist.planrec.rules.HTN.*
import org.maraist.planrec.terms.TermImpl
import org.maraist.planrec.terms.Term.termHead
import TriggerHint.*
import org.maraist.planrec.terms.{>?<, >><<}
import scala.compiletime.ops.any

/**
  * Indirection node at the head of a stack fragment in the lower graph
  *
  * @param goal Goal term for which this substack was spawned.
  */
case class Ind[T, H, S](val goal: T)

// case class NfaAnnotation[T, H, S](indirects: List[H])

// case class DfaAnnotation[T, H, S](indirects: List[(H, Int)])

/**
  * Item annotated with newly-introduced top-level insertion point(s),
  * used in the handle-finding automata when new concurrent subgoals
  * are first actionable.
  *
  * @param indirects The new concurrent subgoals.
  * @param item Underlying item.
  */
case class Sparking[T, H, S](indirects: List[H], item: AllItem[T, H, S]) {
  def itemOf: Option[Item[T, H, S]] = Some(item)
}

// case class StateItem[T, H, S](item: Item[T, H, S])

case class Station[H](head: H)

/**
  * Possible forms of the state in the nondeterminisic handle-finding
  * automaton (or state elements in the deterministic automaton).
  */
type HState[T, H, S] = Sparking[T, H, S] | Item[T, H, S] | Station[H]

/**
  * The specific type of queue for holding states to be processed when
  * building the handle-finding nondeterministic automaton.
  */
type ItemsQueue[T, H, S] =
  Queue[(
    HState[T, H, S],
    Set[Int],
    Option[Int],
    Sparking[T, H, S] | AllItem[T, H, S])]

// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr.table
import scala.collection.mutable.Queue
import org.maraist.planrec.rules.HTNLib
import org.maraist.planrec.rules.HTN.HTNrule
import org.maraist.planrec.terms.Term.*
import org.maraist.planrec.rules.HTN.*
import org.maraist.planrec.yr.table.Item.*
import org.maraist.fa.hyperedges.{HyperedgeNDFA,HyperedgeDFA}
import org.maraist.fa.hyperedges.impl.HashHyperedgeNDFABuilder

case class Ind[T, H, S](val rule: HTNrule[T, H, S])

class Table[T, H, S](val library: HTNLib[T, H, S])(using TermImpl[T, H, S]) {

  // Commenting out while debugging Rule extension methods

  val dfa: HyperedgeDFA[Set[Node[T,H,S]], H] = {
    val nfa = Table.libToNFA(library)
    nfa.toDFA
  }
}

object Table {

  def libToNFA[T, H, S](library: HTNLib[T, H, S])(using TermImpl[T, H, S]):
      HyperedgeNDFA[Node[T,H,S], H, ? <: HyperedgeDFA[Set[Node[T,H,S]], H]] = {
    val nfaBuilder = new HashHyperedgeNDFABuilder[Node[T,H,S], H]()

    // Queue for all of the items which need to be processed
    val itemsQueue = new Queue[(Option[(Item[T, H, S], T)], Item[T, H, S])]

    // Add each rule goal term head as a state
    for (rule <- library.rules) do {
      val ruleGoalHead = rule.goal.termHead

      // First make sure the rule head is a state of the NFA, and is
      // initial if appropriate.
      if nfaBuilder.isState(ruleGoalHead)
      then (if library.top.contains(ruleGoalHead)
                && !nfaBuilder.isInitialState(ruleGoalHead)
            then nfaBuilder.addInitialState(ruleGoalHead))
      else if library.top.contains(ruleGoalHead)
      then nfaBuilder.addInitialState(ruleGoalHead)
      else nfaBuilder.addState(ruleGoalHead)

      // Add the rule station and initial item to the NFA, and queue
      // the initial item for further procesing.  We maintain the
      // invariant that all items in the processing queue are already
      // nodes in the NFA.
      rule.queueInitialRuleForms(itemsQueue, nfaBuilder)
    }

    // Process the items in the queue
    while (!(itemsQueue.isEmpty))
      itemsQueue.dequeue match
        case (prev, item) =>
          item.encodeItemTransition(prev, nfaBuilder, itemsQueue)

    nfaBuilder.toNDFA
  }
}


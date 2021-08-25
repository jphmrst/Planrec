// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr.table
import scala.collection.mutable.Queue
import org.maraist.fa.annotated.
  {EdgeAnnotatedNDFA, NDFAEdgeAnnotationsBuilder,
    HashEdgeAnnotatedNDFABuilder, EdgeAnnotatedDFA, setCombiner}
import org.maraist.planrec.rules.{All,One,Act,TriggerHint,TriggerMatchIndex}
import org.maraist.planrec.rules.HTN.*
import org.maraist.planrec.terms.TermImpl
import org.maraist.planrec.terms.Term.termHead
import TriggerHint.*
import org.maraist.planrec.terms.{>?<, >><<}
import scala.compiletime.ops.any

case class Ind[T, H, S](val rule: HTNrule[T, H, S])

type Node[T, H, S] = Item[T, H, S] | H | Ind[T, H, S]

type HandleFinder[T, H, S] = EdgeAnnotatedDFA[Set[Node[T, H, S]], T, Set[Unit]]

type NondetHandleFinder[T, H, S] =
  EdgeAnnotatedNDFA[
    Node[T, H, S],
    T,
    Unit,
    Set[Unit],
    ? <: HandleFinder[T, H, S]
  ]
type NondetHandleFinderBuilder[T, H, S] =
  NDFAEdgeAnnotationsBuilder[
    Node[T, H, S], T,
    Unit, Set[Unit],
    ?, NondetHandleFinder[T, H, S],
    ?
  ]
type NondetHandleFinderBuilderConcrete[T, H, S] =
  HashEdgeAnnotatedNDFABuilder[Node[T, H, S], T, Set[Unit], Unit]

extension [T, H, S](rule: HTNrule[T, H, S])(using termImpl: TermImpl[T,H,S]) {
  def queueInitialRuleForms(
    queue: Queue[(Option[(Item[T, H, S], T)], Item[T, H, S])],
    nfa: NondetHandleFinderBuilder[T, H, S]):
      Unit =
    rule match {
      case allRule: All[T, H, S] => {
      }
      case oneRule: One[T, H, S] => {
      }
      case actRule: Act[T, H, S] => {
      }
    }
}

object HandleFinder {
  import org.maraist.planrec.rules.HTNLib

  def libToNFA[T, H, S](library: HTNLib[T, H, S])(using TermImpl[T, H, S]):
      NondetHandleFinder[T, H, S] = {
    val nfaBuilder = new NondetHandleFinderBuilderConcrete[T, H, S]()

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
      itemsQueue.enqueue((None, initialItem(rule)))
    }

    // Process the items in the queue
    while (!(itemsQueue.isEmpty))
      itemsQueue.dequeue match
        case (prev, item) =>
          encodeItemTransition(prev, item, nfaBuilder, itemsQueue)

    nfaBuilder.toNDFA
  }

  /** Add the necessary components to an [[NDFABuilder]] for the `next`
    * item (which may already have been added to the NFA), and for the
    * transition from the `prev` item to the `next` item.
    *
    * @param item The item being added to the NFA.
    * @param queue Queue of items to subsequently be added to the NFA.
    * Additional items pairs may be pushed to the `queue`, but no
    * pairs should be read from it.
    */
  def encodeItemNode[T, H, S](
    item: Item[T, H, S],
    nfa: NondetHandleFinderBuilder[T, H, S],
    queue: Queue[(Option[(Item[T, H, S], T)], Item[T, H, S])]):
      Unit = item match {
    case allItem @ (AllItem(rule, ready)) => {
      // Add the item as a node if it is not already in the NDA.
      if !(nfa.isState(allItem)) then nfa.addState(allItem)
    }

    case OneItem(rule, isFinal) => {
      ???
    }

    case ActItem(rule, isFinal) => {
      ???
    }
  }

  /** Add the necessary components to an [[NDFABuilder]] for the `next`
    * item (which may already have been added to the NFA), and for the
    * transition from the `prev` item to the `next` item.
    *
    * @param prev Item from which the `next` item was derived, if any.
    * @param next The item being added to the NFA.
    * @param queue Queue of items to subsequently be added to the NFA.
    * Additional items pairs may be pushed to the `queue`, but no
    * pairs should be read from it.
    */
  def encodeItemTransition[T, H, S](
    prev: Option[(Item[T, H, S], T)],
    next: Item[T, H, S],
    nfa: NondetHandleFinderBuilder[T, H, S],
    queue: Queue[(Option[(Item[T, H, S], T)], Item[T, H, S])]):
      Unit = next match {
    case allItem @ (AllItem(rule, ready)) => {

      val prior = prev match {
        case None => ()
        case Some(item, trigger) => ()
      }
      // If there is only one ready element, then we can just add a
      // simple item.  ***BUT*** we need a trigger for the transition
      // itself.

      // Otherwise if there was only one ready element in the `prev`,
      // or if there is no `prev`, then we need to add an indirection
      // node since we now must synchronize two possibly-interleaved
      // subgoals.

      // Moreover if we have more than one element of the ready set,
      // and it is also not a subset of the predecessor, then we need
      // to annotate with concurrent stack top spawning.

      ???
    }

    case OneItem(rule, isFinal) => {
      ???
    }

    case ActItem(rule, isFinal) => {
      ???
    }
  }
}

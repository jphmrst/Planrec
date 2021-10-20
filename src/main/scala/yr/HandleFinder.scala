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
import org.maraist.graphviz.Graphable
import org.maraist.fa.
  {EdgeAnnotatedNFA, EdgeAnnotatedNFABuilder, EdgeAnnotatedDFA, setCombiner}
import org.maraist.fa.util.setCombiner
import org.maraist.planrec.rules.{All,One,Act,TriggerHint,TriggerMatchIndex}
import org.maraist.planrec.rules.HTN.*
import org.maraist.planrec.terms.TermImpl
import org.maraist.planrec.terms.Term.termHead
import TriggerHint.*
import org.maraist.planrec.terms.{>?<, >><<}
import scala.compiletime.ops.any

case class Ind[T, H, S](val rule: HTNrule[T, H, S])

case class NfaAnnotation[T, H, S](indirects: List[H])

type Node[T, H, S] = Item[T, H, S] | H | Ind[T, H, S]

type HandleFinder[T, H, S] =
  EdgeAnnotatedDFA[
    Set[Node[T, H, S]],
    H,
    Set[NfaAnnotation[T, H, S]]
  ]

type NondetHandleFinder[T, H, S] =
  EdgeAnnotatedNFA[
    Node[T, H, S], H,
    NfaAnnotation[T, H, S], Set[NfaAnnotation[T, H, S]]
  ]

type NondetHandleFinderBuilder[T, H, S] =
  EdgeAnnotatedNFABuilder[
    Node[T, H, S], H,
    NfaAnnotation[T, H, S], Set[NfaAnnotation[T, H, S]]
  ]

type NondetHandleFinderBuilderConcrete[T, H, S] =
  EdgeAnnotatedNFABuilder[
    Node[T, H, S],
    H,
    NfaAnnotation[T, H, S], Set[NfaAnnotation[T, H, S]]
  ]

type ItemsQueue[T, H, S] =
  Queue[(Node[T, H, S], Set[Int], Option[Int], AllItem[T, H, S])]

object HandleFinder {
  import org.maraist.fa.util.EdgeAnnotationCombiner.singleSetCombiner
  import org.maraist.planrec.rules.HTNLib

  def libToNFA[T, H, S](library: HTNLib[T, H, S])(using TermImpl[T, H, S]):
      NondetHandleFinder[T, H, S] = {
    val nfaBuilder =
      new NondetHandleFinderBuilderConcrete[T, H, S](
        using singleSetCombiner[NfaAnnotation[T, H, S]])

    // First add all of the rule heads as stations.
    for (head <- library.goals.map(_.termHead))
      do {
        if library.top.contains(head)
        then nfaBuilder.addInitialState(head)
        else nfaBuilder.addState(head)
      }

    // Queue for all of the items which need to be processed.
    val itemsQueue = new ItemsQueue[T, H, S]

    // Process each rule, or stage it for processing.
    for (rule <- library.rules) do {
      val ruleGoalHead = rule.goal.termHead
      val initial = initialItem(rule)

      // Add the rule station and initial item to the NFA, with an
      // epsilon transition from the station to that item.
      nfaBuilder.addState(initial)
      nfaBuilder.addETransition(ruleGoalHead, initial)

      // For One- and Act-rules, we can just add the related items
      // right away.  Otherwise for All-rules, we queue the initial
      // item.  Note that we maintain the invariant that all items in
      // the processing queue are already nodes in the NFA.
      rule match {
        case r @ One(_, subgoals, probs) => { // TODO Do something with probs
          val finalItem = OneItem(r, true)
          nfaBuilder.addFinalState(finalItem)
          // OK to use triggers instead of actionHints here because
          // they all lead to the finalItem.
          for (t <- initial.triggers)
            do nfaBuilder.addTransition(initial, t, finalItem)
          for (s <- subgoals)
            do nfaBuilder.addETransition(initial, s.termHead)
        }
        case r: Act[T, H, S] => {
          val finalItem = ActItem(r, true)
          nfaBuilder.addFinalState(finalItem)
          nfaBuilder.addTransition(initial, r.action.termHead, finalItem)
        }
        case r: All[T, H, S] => {
          initial match {
            case i: AllItem[T, H, S] => {
              val isMulti = (i.actionHints.size > 1)

              // If not multi, then add annotations to the epsilon
              // transition for the subgoals.
              if isMulti then
                nfaBuilder.setEAnnotation(
                  ruleGoalHead, initial, NfaAnnotation(List.from(i.triggers))
                )

              // Set up this initial item for mapping out its
              // successors.
              itemsQueue.enqueue((ruleGoalHead, Set.empty[Int], None, i))
            }

            case _ => { } // Other cases not possible, even though
                          // Scala cannot detect.
          }
        }
      }
    }

    // Process the items in the queue
    while (!(itemsQueue.isEmpty))
      itemsQueue.dequeue match
        case (prev, par, trans, item) =>
          encodeItemTransition(prev, par, trans, item, nfaBuilder, itemsQueue)

    nfaBuilder.result
  }

  /** Add the necessary components to an [[NDFABuilder]] for the
    * transition from a `prev` item to the `next` item, and queue up
    * any succeeding transitions.
    *
    * @param prev Item from which the `next` item was derived, if any.
    * @param next The item being added to the NFA.
    * @param queue Queue of items to subsequently be added to the NFA.
    * Additional items pairs may be pushed to the `queue`, but no
    * pairs should be read from it.
    */
  def encodeItemTransition[T, H, S](
    prev: Node[T, H, S],
    par: Set[Int],
    transIdx: Option[Int],
    nextItem: AllItem[T, H, S],
    nfa: NondetHandleFinderBuilder[T, H, S],
    queue: ItemsQueue[T, H, S])(
    using TermImpl[T, H, S]):
      Unit = {
    val rule = nextItem.rule

    // If the item is final, mark it as a final state.
    if (nextItem.isFinal) then nfa.addFinalState(nextItem)

    // Work out the current spawned tasks _minus_ any in the
    // transition.
    val postTransPar = transIdx match {
      case None => par
      case Some(i) => par - i
    }

    // If there are any spawned tasks left, then nextItem is catching
    // indirected subgoals.
    val wasMulti = !postTransPar.isEmpty

    // We will also need to examine any triggers in the nextItem's
    // ready set which are not already spawned out.
    val newInNextItem = nextItem.ready -- postTransPar

    // There is already a transition between prev and nextItem, but we
    // may need to annotate it.
    if ((wasMulti && newInNextItem.size > 0) || newInNextItem.size > 1)
      then transIdx match {
        case None => nfa.setEAnnotation(
          prev, nextItem,
          NfaAnnotation(List.from(newInNextItem.map(rule.subgoals(_).termHead)))
        )
        case Some(idx) => nfa.setAnnotation(
          prev, rule.subgoals(idx).termHead, nextItem,
          NfaAnnotation(
            List.from(newInNextItem.map((i) => rule.subgoals(i).termHead)))
        )
      }

    // Calculate the set of spawned terms active with nextItem
    val parAfterNext = if (wasMulti || newInNextItem.size > 1) then {
      postTransPar ++ newInNextItem
    } else {
      Set.empty[Int]
    }


    // TODO Check if nextItem has already been expanded --- skip the
    // next bits if so.

    // If we are not spawning the newly enabled subgoals, then we
    // epsilon-transition to its station.
    if (!wasMulti && newInNextItem.size <= 1) then {
      newInNextItem.map(
        (idx) => nfa.addETransition(nextItem, rule.subgoals(idx).termHead))
    }

    // We have (at least) a new queue entry for each subgoal which is
    // ready in the new item.
    for (newTransIdx <- nextItem.ready) do {
      val newTransTerm = rule.subgoals(newTransIdx)
      val newTrans = newTransTerm.termHead

      // Build the resulting item, and add it to the NFA if it is not
      // already there.
      nextItem.applyIdx(newTransIdx) match {
        case None => { }
        case Some(afterNextItem) => {
          // Add the transition
          nfa.addTransition(nextItem, newTrans, afterNextItem)

          // Add a queue entry
          queue.enqueue((nextItem, parAfterNext, Some(newTransIdx), afterNextItem))
        }
      }
    }
  }
}
